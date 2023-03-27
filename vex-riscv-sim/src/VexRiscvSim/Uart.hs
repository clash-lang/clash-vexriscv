{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
module VexRiscvSim.Uart where

import Clash.Prelude

import Clash.Cores.UART ( uart, ValidBaud )
import Clash.Signal.Internal (Signal ((:-)), fromList)

import Protocols.Wishbone
import Bittide.SharedTypes (Bytes)
import Data.String.Interpolate (i)
import Data.Maybe
import Data.List.NonEmpty as NonEmpty
import System.IO
import qualified Data.List as L
import System.IO.Unsafe (unsafePerformIO)
import Data.Char (chr, ord)
import Debug.Trace (trace)

{-# NOINLINE fifo #-}

-- | Simple First in, First out buffer the contains a backpressure mechanism.
fifo ::
  forall dom fifoDepth a .
  (HiddenClockResetEnable dom,  2 <= fifoDepth, NFDataX a) =>
  SNat fifoDepth ->
  Signal dom (Maybe a) ->
  Signal dom Bool ->
  ( Signal dom (Maybe a)
  , Signal dom Bool)
fifo depth@SNat fifoIn readyIn = (fifoOut, readyOut)
 where
  bramOut = readNew (blockRamU NoClearOnReset depth (const undefined)) readAddr writeOp
  (readAddr, writeOp, fifoOut, readyOut) = mealyB go (0, 0, True) (fifoIn, readyIn, bramOut)
  go ::
    (Index fifoDepth, Index fifoDepth, Bool) ->
    (Maybe a, Bool, a) ->
    ( (Index fifoDepth, Index fifoDepth, Bool)
    , (Index fifoDepth, Maybe (Index fifoDepth, a), Maybe a, Bool))
  go (readCounter, writeCounter,fifoEmpty) (fifoInGo, readyInGo, bramOutGo) =
    ((readCounterNext, writeCounterNext, fifoEmptyNext), output)
   where
    readSucc  = satSucc SatWrap readCounter
    writeSucc = satSucc SatWrap writeCounter

    readCounterNext
      | not fifoEmpty && readyInGo = readSucc
      | fifoEmpty && readyInGo     = error "read on an empty fifo"
      | otherwise                  = readCounter

    (writeCounterNext, writeOpGo)
      | not full && isJust fifoInGo = (writeSucc, (writeCounter,) <$> fifoInGo)
      | full && isJust fifoInGo     = error "write on a full fifo"
      | otherwise                   = (writeCounter, Nothing)

    countersSame     = readCounter == writeCounter
    nextCountersSame = readCounterNext == writeCounterNext

    full = not fifoEmpty && countersSame
    fifoEmptyNext
      | fifoEmpty = countersSame
      | otherwise = readyInGo && nextCountersSame
    fifoOutGo
      | fifoEmpty = Nothing
      | otherwise = Just bramOutGo

    output = (readCounterNext, writeOpGo, fifoOutGo, not full)



-- | Wishbone accessible UART core featuring a transmit `fifo`.
uartWb ::
  forall dom addrW nBytes baudRate transmitBufferDepth receiveBufferDepth .
  ( HiddenClockResetEnable dom, ValidBaud dom baudRate
  , 2 <= transmitBufferDepth
  , 2 <= receiveBufferDepth
  , 2 <= addrW
  , KnownNat addrW
  , KnownNat nBytes
  ) =>
  SNat transmitBufferDepth ->
  SNat receiveBufferDepth ->
  SNat baudRate ->
  Signal dom (WishboneM2S addrW nBytes (Bytes nBytes)) ->
  Signal dom Bit ->
  -- |
  -- 1. Wishbone slave bus
  -- 2. (txFull, txEmpty, rxFull, rxFifoEmpty)
  -- 3. Uart TX Serial
  ( Signal dom (WishboneS2M(Bytes nBytes))
  , Signal dom (Bool, Bool, Bool, Bool)
  , Signal dom Bit)
uartWb txDepth@SNat rxDepth@SNat baud wbM2S uartRx = (wbS2M, uartStatus, uartTx)
 where
  (uartReceived, uartTx, uartAck) = uart baud uartRx uartRequest
  (uartRequest, txFifoReady) = fifo txDepth txFifoWrite uartAck
  (rxFifoStored, rxFifoReady) = fifo rxDepth uartReceived rxAck
  (wbS2M, txFifoWrite, rxAck) = unbundle $ fmap go (bundle (wbM2S, rxFifoStored, uartStatus))
  uartStatus = bundle
    ( not <$> txFifoReady
    , isNothing <$> uartRequest
    , not <$> rxFifoReady
    , isNothing <$> rxFifoStored
    )
  go ( WishboneM2S{..}, rxData, uartStatusGo) =
    ((emptyWishboneS2M @()){acknowledge, err, readData} , txWrite, validRead)
   where
    (alignedAddr, alignment) = split @_ @(addrW - 2) @2 addr
    internalAddr = bitCoerce $ resize alignedAddr :: Index 2
    addrLegal = alignedAddr <= 1 && alignment == 0
    validRead = masterActive && internalAddr == 0 && not writeEnable
    validWrite = masterActive && internalAddr == 0 && writeEnable
    validReadStatus = masterActive && internalAddr == 1 && not writeEnable

    masterActive = busCycle && strobe
    acknowledge = (validRead || validWrite || validReadStatus) && addrLegal
    err = masterActive && (not acknowledge || not addrLegal)
    readData
      | validRead       = resize $ fromMaybe 0 rxData
      | validReadStatus = resize $ pack uartStatusGo
      | otherwise       = deepErrorX
        [i|"uartWb: Invalid request.
        BUS: {busCycle}
        STR: {strobe}
        ADDR: {addr}
        WE:{writeEnable}
        ACK:{acknowledge}
        ERR:{err}"|]

    txWrite
      | validWrite = Just $ resize writeData
      | otherwise  = Nothing




uartIO ::
  forall dom baud .
  ( HiddenClockResetEnable dom
  , ValidBaud dom baud) =>
  Handle ->
  Handle ->
  SNat baud ->
  (Signal dom Bit -> Signal dom Bit) ->
  IO ()
uartIO inputHandle outputHandle baud uartDevice = printList . catMaybes $ sample echoServer
 where

  printList :: [BitVector 8] -> IO ()
  printList [] = pure ()
  printList (x:xs) = do
    hPutChar outputHandle . chr $ fromIntegral x
    hFlush outputHandle
    printList xs

  uartRx = uartDevice uartTx

  (unbundle -> (echoServer, uartTx)) = runUart baud uartRx $ fmap (fmap (fromIntegral . ord)) input
  input = unsafePerformIO <$> ioList inputHandle

ioList :: Handle -> [IO (Maybe Char)]
ioList h = flip (:) (ioList h) $ do
  charReady <- hReady h
  if charReady
  then Just <$> hGetChar h
  else pure Nothing

{-
f ::
  a ->
  -- ^ state
  [Maybe (BitVector 8)] ->
  -- ^ all inputs since last ran
  ([BitVector 8], a)
  -- ^ Bytes to write (can be []), next state
-}


data UartState
  = NoDataToSend
  | WaitForAck [BitVector 8] [Maybe (BitVector 8)]
  -- ^        bytes to send    inputs from remote since last call
  deriving (Generic, NFDataX)

uartState ::
  forall dom baud a .
  ( HiddenClockResetEnable dom
  , ValidBaud dom baud
  , NFDataX a ) =>
  SNat baud ->
  a ->
  -- ^ initial state
  (a -> [Maybe (BitVector 8)] -> ([BitVector 8], a)) ->
  -- ^ state function
  Signal dom Bit ->
  Signal dom (Maybe (BitVector 8), Bit)
uartState baud initSt stateFn uartRx = bundle (uartReceived, uartTx)
  where
    (uartReceived, uartTx, uartAck) = uart baud uartRx send

    send = mealyB go (NoDataToSend, initSt) (uartAck, uartReceived)

    -- start state
    go (NoDataToSend, st0) (_ack, recv) =
      case stateFn st0 [recv] of
          ([], st1) -> ((NoDataToSend, st1), Nothing)
          (d:ds, st1) -> ((WaitForAck (d:ds) [], st1), Just d)
    -- last byte sent
    go (WaitForAck [] _, _) _ = errorX "Unreachable"
    go (WaitForAck [_] recs, st0) (True, recv) =
      case stateFn st0 (recv:recs) of
          ([], st1) -> ((NoDataToSend, st1), Nothing)
          (d:ds, st1) -> ((WaitForAck (d:ds) [], st1), Just d)
    -- byte in sequence was ACKd
    go (WaitForAck (_:d:ds) recs, st0) (True, recv) =
      ((WaitForAck (d:ds) (recv:recs), st0), Just d)
    -- no ACK
    go (WaitForAck (d:ds) recs, st0) (False, recv) = ((WaitForAck (d:ds) (recv:recs), st0), Just d)




runUart ::
  forall dom baud .
  ( HiddenClockResetEnable dom
  , ValidBaud dom baud) =>
  SNat baud ->
  Signal dom Bit ->
  [Maybe (BitVector 8)] ->
  Signal dom (Maybe (BitVector 8), Bit)
runUart baud uartRx sendBytes = bundle (uartReceived, uartTx)
 where
  (uartReceived, uartTx, uartAck) = uart baud uartRx uartSend
  uartSend = andEnable (isNothing <$> uartSend .||. uartAck) (fromListWithResetAndEnable $ NonEmpty.fromList sendBytes)

-- | Converts a list of elements into a signal of elements. Unlike 'fromList'
-- it also takes a clock, reset, and enable. When the reset is asserted, it
-- will insert the first value of the given list. When the enable is deasserted,
-- elements are repeated. As usual, the reset takes precedence over the enable.
--
-- __NB__: Not translatable to HDL
fromListWithResetAndEnable ::
  forall dom a .
  (HiddenClockResetEnable dom, NFDataX a, Show a) =>
  NonEmpty a ->
  Signal dom a
fromListWithResetAndEnable inp =
  go
    (fromEnable hasEnable)
    (unsafeToHighPolarity hasReset)
    inpAsSignal

 where
  eolError = deepErrorX "fromListWithResetAndEnable: end of list"
  inpAsSignal = Clash.Signal.Internal.fromList (NonEmpty.toList inp <> L.repeat eolError)

  go :: Signal dom Bool -> Signal dom Bool -> Signal dom a -> Signal dom a
  go (ena :- enables) (rst :- resets) (x :- xs) =
    output :- go enables resets remaining
   where
    output
      | rst       = NonEmpty.head inp
      | otherwise = x
    remaining
      | rst       = inpAsSignal
      | ena       = xs
      | otherwise = x :- xs
