-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

import Bittide.DoubleBufferedRam (ContentType (..), InitialContent (..), wbStorage')
import Bittide.SharedTypes hiding (delayControls)
import Bittide.Wishbone (singleMasterInterconnect')
import Clash.Explicit.BlockRam.File (memFile)
import Clash.Prelude hiding (not, (&&))
import Clash.Signal.Internal (Signal ((:-)), DomainConfigurationPeriod)
import Control.Exception (bracket)
import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.IntMap as I
import qualified Data.List as L
import Data.Maybe (catMaybes, isJust)
import GHC.Base (assert)
import GHC.IO.Handle
import GHC.Stack (HasCallStack)
import qualified GHC.TypeNats as TN
import Protocols.Wishbone
import System.Directory (removeFile)
import System.Environment
import System.IO (openTempFile, stdout, stdin)
import Text.Printf
import Utils.ReadElf
import VexRiscv
import Prelude as P hiding ((||))
import Data.Char (chr)
import Clash.Cores.UART (ValidBaud, uart)
import VexRiscvSim.Uart (uartWb, uartIO, runUart, uartState)
import Debug.Trace (trace)
import Data.Digest.Pure.MD5 (md5, md5DigestBytes)
import Numeric (showHex)

--------------------------------------
--
-- Debugging configuration
--
--------------------------------------

data DebugConfiguration where
  -- | Run a program and only output what the program writes to the
  --   character-device
  RunCharacterDevice :: DebugConfiguration
  -- | Run a program and print detailed information of CPU bus interactions
  InspectBusses ::
    -- | # of cycles the program takes to initialise the instruction memory
    Int ->
    -- | # of "uninteresting" cycles to skip, such as runtime setup code
    Int ->
    -- | # of "interesting" cycles to inspect
    Maybe Int ->
    -- | inspect instruct-bus interactions
    Bool ->
    -- | inspect data-bus interactions
    Bool ->
    DebugConfiguration
  -- | Run a program and output all the write operations
  InspectWrites :: DebugConfiguration

-- change this variable to the configuration you want to use

debugConfig :: DebugConfiguration
debugConfig =
  -- InspectWrites
  RunCharacterDevice

--
{-
  InspectBusses
    64800
    0
    (Just 200)
    True
    True
-- -}

--------------------------------------

emptyInput :: Input
emptyInput =
  Input
    { timerInterrupt = low,
      externalInterrupt = low,
      softwareInterrupt = low,
      iBusWbS2M = (emptyWishboneS2M @(BitVector 32)) {readData = 0},
      dBusWbS2M = (emptyWishboneS2M @(BitVector 32)) {readData = 0}
    }

type Memory dom =
  ( BitVector 32,
    Signal dom (WishboneM2S 32 4 (BitVector 32)) ->
    Signal dom (WishboneS2M (BitVector 32))
  )

-- When passing S2M values from Haskell to VexRiscv over the FFI, undefined
-- bits/values cause errors when forcing their evaluation to something that can
-- be passed through the FFI.
--
-- This function makes sure the Wishbone S2M values are free from undefined bits.
makeDefined :: WishboneS2M (BitVector 32) -> WishboneS2M (BitVector 32)
makeDefined wb = wb {readData = defaultX 0 (readData wb)}

defaultX :: (NFDataX a) => a -> a -> a
defaultX dflt val
  | hasUndefined val = dflt
  | otherwise = val

{-
data UartLoaderState
  = UWaitingForStart
  | UReadPreamble String
  | UWriteSize [Byte]
  | UWritePayload [Byte]
  deriving (Generic, NFDataX)

uartLoaderComponent ::
  forall dom baud.
  (HiddenClockResetEnable dom, ValidBaud dom baud) =>
  [Byte] ->
  -- ^ Payload
  SNat baud ->
  Signal dom Bit ->
  Signal dom Bit
uartLoaderComponent payload baud rx = tx
  where
    loaderOut = uartLoader payload rxDat txSuccess
    (rxDat, tx, txSuccess) = uart baud rx loaderOut

uartLoader ::
  (HiddenClockResetEnable dom) =>
  [Byte] ->
  -- ^ Payload
  Signal dom (Maybe Byte) ->
  -- ^ data from remote
  Signal dom Bool ->
  -- ^ signal if send was successful
  Signal dom (Maybe Byte)
  -- ^ write output
uartLoader payload readByte txSuccess = outSignal
  where
    outSignal = mealyB go UWaitingForStart (readByte, canWrite)
    justWritten = isJust <$> outSignal
    writeJustDone = txSuccess
    waitingForWriteAck = register False $
      mux waitingForWriteAck
        -- True
        (mux writeJustDone (pure False) (pure True))
        -- False
        (mux justWritten (pure True) (pure False))
    canWrite = not <$> waitingForWriteAck

    payloadSize = fromIntegral $ L.length payload :: BitVector 32
    payloadSizeBytesLe =
      let (a :: Byte, b :: Byte, c :: Byte, d :: Byte) = unpack payloadSize
      in [d, c, b, a]

    go :: UartLoaderState -> (Maybe Byte, Bool) -> (UartLoaderState, Maybe (BitVector 8))
    go UWaitingForStart (Nothing, _) = (UWaitingForStart, Nothing)
    go UWaitingForStart (Just (chr . fromEnum -> b), _) = trace ("received a character: " <> show b) (UReadPreamble [b], Nothing)
    -- adding characters to the front, so it's backwards
    go (UReadPreamble "dl") (Just (chr . fromEnum -> '\n'), _) = trace "done with preamble!" (UWriteSize payloadSizeBytesLe, Nothing)
    go (UReadPreamble buf) (Just (chr . fromEnum -> b), _) = trace ("received a character [" <> show buf <> "]: " <> show b) (UReadPreamble (b:buf), Nothing)
    go (UReadPreamble buf) (Nothing, _) = (UReadPreamble buf, Nothing)

    go (UWriteSize []) (_, _) = trace "done writing size" (UWritePayload payload, Nothing)
    go (UWriteSize bs) (_, False) = (UWriteSize bs, Nothing)
    go (UWriteSize (b:bs)) (_, True) = trace "writing size byte" (UWriteSize bs, Just b)

    go (UWritePayload []) (_, _) = (UWaitingForStart, Nothing)
    go (UWritePayload bs) (_, False) = (UWritePayload bs, Nothing)
    go (UWritePayload (b:bs)) (_, True) = (UWritePayload bs, Just b)

-}


{-
Address space

0b0000 0x0000_0000 UART Addresses (prefixes 0b000x)
0b0010 0x2000_0000 Boot instruction memory
0b0100 0x4000_0000 Boot data memory
0b0110 0x6000_0000 Loaded instruction memory
0b1000 0x8000_0000 Loaded data memory

for instruction bus

0b000 0x0 dummy
0b001 0x2 boot instructions
0b010 0x4 dummy
0b011 0x6 loaded instruction memory
0b100 0x8 dummy

for data bus

0b000 0x0 dummy
0b001 0x2 dummy
0b010 0x4 boot data memory
0b011 0x6 loaded instruction memory
0b100 0x8 loaded data memory

-}

cpu ::
  forall dom baud .
  (HasCallStack, HiddenClockResetEnable dom
  , 1 <= DomainConfigurationPeriod (KnownConf dom)
  , ValidBaud dom baud) =>
  Memory dom ->
  Memory dom ->
  SNat baud ->
  -- Term IO UART receive line
  Signal dom Bit ->
  -- bootloader UART receive line
  Signal dom Bit ->
  ( Signal dom Output,
    -- writes
    Signal dom (Maybe (BitVector 32, BitVector 32)),
    -- iBus responses
    Signal dom (WishboneS2M (BitVector 32)),
    -- dBus responses
    Signal dom (WishboneS2M (BitVector 32)),
    -- Term IO UART transmit line
    Signal dom Bit,
    -- bootlader UART transmit line
    Signal dom Bit
  )
cpu (_iMemStart, bootIMem) (_dMemStart, bootDMem) baud termUartRx bootUartRx = (output, writes, iS2M, dS2M, termUartTx, bootUartTx)
  where
    output = vexRiscv (emptyInput :- input)
    dM2S = dBusWbM2S <$> output

    -- 0x0100_0000
    (termUartS2M, _, termUartTx) =
        uartWb (SNat @32) (SNat @32) baud
          (mapAddr @28 @29 ((\a -> a - 0x0100_0000) . resize) <$> termUartM2S)
          termUartRx
    
    -- 0x1000_0000
    (bootUartS2M, _, bootUartTx) =
        uartWb (SNat @16) (SNat @512) baud
          (mapAddr @28 @29 resize <$> bootUartM2S)
          bootUartRx
    
    (uartS2M, unbundle -> (termUartM2S :> bootUartM2S :> Nil)) =
      singleMasterInterconnect'
        -- 1 bit prefix
        (0b0 :> 0b1 :> Nil)
        uartM2S
        (bundle (termUartS2M :> bootUartS2M :> Nil))

    errS2M = emptyWishboneS2M {err = True}

    (iS2M, unbundle -> (_ :> bootIM2S :> _ :> loadedIM2S :> _ :> Nil)) =
      singleMasterInterconnect'
        -- 3 bit prefix
        (0b000 :> 0b001 :> 0b010 :> 0b011 :> 0b100 :> Nil)
        (unBusAddr . iBusWbM2S <$> output)
        (bundle (pure errS2M :> bootIS2M :> pure errS2M :> loadedIS2M :> pure errS2M :> Nil))

    bootIS2M = bootIMem (mapAddr @29 @32 resize <$> bootIM2S)
    (loadedIS2M, loadedIS2MDbus) =
      dualPortStorage
        (Undefined :: InitialContent (512 * 1024) (Bytes 4))
        -- port A, prioritised, instruction bus
        (mapAddr @29 @32 resize <$> loadedIM2S)
        -- port B, data bus
        (mapAddr @29 @32 resize <$> loadedIM2SDbus)

    bootDS2M = bootDMem (mapAddr @29 @32 resize <$> bootDM2S)
    loadedDS2M = wbStorage' (Undefined :: InitialContent (512 * 1024) (Bytes 4)) (mapAddr @29 @32 resize <$> loadedDM2S)

    (dS2M, unbundle -> (uartM2S :> _ :> bootDM2S :> loadedIM2SDbus :> loadedDM2S :> Nil)) =
      singleMasterInterconnect'
        -- 3 bit prefix
        (0b000 :> 0b001 :> 0b010 :> 0b011 :> 0b100 :> Nil)
        (unBusAddr . dBusWbM2S <$> output)
        ( bundle
            (uartS2M :> pure errS2M :> bootDS2M :> loadedIS2MDbus :> loadedDS2M :> Nil)
        )

    input =
      ( \iBus dBus ->
          Input
            { timerInterrupt = low,
              externalInterrupt = low,
              softwareInterrupt = low,
              iBusWbS2M = makeDefined iBus,
              dBusWbS2M = makeDefined dBus
            }
      )
        <$> iS2M
        <*> dS2M

    unBusAddr = mapAddr ((`shiftL` 2) . extend @_ @_ @2)

    writes =
      mux
        ( (busCycle <$> dM2S)
            .&&. (strobe <$> dM2S)
            .&&. (writeEnable <$> dM2S)
            .&&. (acknowledge <$> dS2M)
        )
        ( do
            dM2S' <- dM2S
            pure $ Just (extend (addr dM2S') `shiftL` 2, writeData dM2S')
        )
        (pure Nothing)

mapAddr :: (BitVector aw1 -> BitVector aw2) -> WishboneM2S aw1 selWidth a -> WishboneM2S aw2 selWidth a
mapAddr f wb = wb {addr = f (addr wb)}


loadProgram :: (HiddenClockResetEnable dom) => FilePath -> IO (IO (), Memory dom, Memory dom)
loadProgram path = do
  elfBytes <- BS.readFile path
  let (entry, iMem, dMem) = readElfFromMemory elfBytes

  assert (entry == 0x2000_0000) (pure ())

  (i0Path, i0Handle) <- openTempFile "/tmp" "imem.0.blob"
  (i1Path, i1Handle) <- openTempFile "/tmp" "imem.1.blob"
  (i2Path, i2Handle) <- openTempFile "/tmp" "imem.2.blob"
  (i3Path, i3Handle) <- openTempFile "/tmp" "imem.3.blob"

  (d0Path, d0Handle) <- openTempFile "/tmp" "dmem.0.blob"
  (d1Path, d1Handle) <- openTempFile "/tmp" "dmem.1.blob"
  (d2Path, d2Handle) <- openTempFile "/tmp" "dmem.2.blob"
  (d3Path, d3Handle) <- openTempFile "/tmp" "dmem.3.blob"

  let removeFiles = mapM_ removeFile [i0Path, i1Path, i2Path, i3Path, d0Path, d1Path, d2Path, d3Path]

  let -- endian swap instructions
      (iL0, iL1, iL2, iL3) = split4 $ content iMem <> [0, 0, 0, 0, 0, 0, 0, 0]
      iMem0BS = memFile Nothing iL0
      iMem1BS = memFile Nothing iL1
      iMem2BS = memFile Nothing iL2
      iMem3BS = memFile Nothing iL3

      (dL0, dL1, dL2, dL3) = split4 $ content dMem
      dMem0BS = memFile Nothing dL0
      dMem1BS = memFile Nothing dL1
      dMem2BS = memFile Nothing dL2
      dMem3BS = memFile Nothing dL3

      iMemStart = startAddr iMem
      dMemStart = startAddr dMem

      iMemSize = (I.size iMem + 8) `divRU` 4
      dMemSize = I.size dMem `divRU` 4

      iContentVec = i0Path :> i1Path :> i2Path :> i3Path :> Nil
      dContentVec = d0Path :> d1Path :> d2Path :> d3Path :> Nil

  assert (dMemStart == 0x4000_0000) (pure ())

  -- write data to files
  -- endian swap data
  hPutStr i0Handle iMem3BS
  hPutStr i1Handle iMem2BS
  hPutStr i2Handle iMem1BS
  hPutStr i3Handle iMem0BS

  hPutStr d0Handle dMem3BS
  hPutStr d1Handle dMem2BS
  hPutStr d2Handle dMem1BS
  hPutStr d3Handle dMem0BS

  -- close files
  hClose i0Handle
  hClose i1Handle
  hClose i2Handle
  hClose i3Handle
  hClose d0Handle
  hClose d1Handle
  hClose d2Handle
  hClose d3Handle

  let instrMem = case TN.someNatVal (toEnum iMemSize) of
        SomeNat (snatProxy -> depth) ->
          case compareSNat depth d1 of
            SNatLE -> error "should not happen"
            SNatGT ->
              let initContent = helper depth $ NonReloadable $ FileVec iContentVec
               in (iMemStart, wbStorage' initContent)

      dataMem = case TN.someNatVal (toEnum dMemSize) of
        SomeNat (snatProxy -> depth) ->
          case compareSNat depth d1 of
            SNatLE -> error "should not happen"
            SNatGT ->
              let initContent = helper depth $ NonReloadable $ FileVec dContentVec
               in (dMemStart, wbStorage' initContent)

  pure (removeFiles, instrMem, dataMem)
  where
    helper ::
      SNat depth ->
      InitialContent depth (BitVector 32) ->
      InitialContent depth (BitVector 32)
    helper SNat cont = cont

    startAddr :: BinaryData -> BitVector 32
    startAddr bin = resize . bitCoerce $ fst . L.head $ I.toAscList bin

    content :: BinaryData -> [BitVector 8]
    content bin = L.map snd $ I.toAscList bin

    split4 :: [BitVector 8] -> ([BitVector 8], [BitVector 8], [BitVector 8], [BitVector 8])
    split4 xs = L.unzip4 $ L.map (\[a, b, c, d] -> (a, b, c, d)) $ chunkFill 4 0 xs

    chunkFill :: Int -> a -> [a] -> [[a]]
    chunkFill _ _ [] = []
    chunkFill n fill xs =
      let (first0, rest) = L.splitAt n xs
          first1 = first0 <> L.replicate (n - L.length first0) fill
       in first1 : chunkFill n fill rest

dualPortStorage ::
  forall dom n addrWidth .
  (HiddenClockResetEnable dom, KnownNat n, KnownNat addrWidth, 1 <= n, 2 <= addrWidth) =>
  InitialContent n (Bytes 4) ->
  -- port A
  Signal dom (WishboneM2S addrWidth 4 (Bytes 4)) ->
  -- port B
  Signal dom (WishboneM2S addrWidth 4 (Bytes 4)) ->
  ( Signal dom (WishboneS2M (Bytes 4)) -- port A
  , Signal dom (WishboneS2M (Bytes 4)) -- port B
  )
dualPortStorage initContent aM2S bM2S = (aS2M, bS2M)
  where
    (unbundle -> (aS2M, bS2M)) = mux bSelected
      (bundle (pure emptyWishboneS2M, ramOut))
      (bundle (ramOut, pure emptyWishboneS2M))

    active m2s = busCycle m2s && strobe m2s

    -- when ONLY B is active, use B, otherwise prioritize A
    bSelected = (not . active <$> aM2S) .&&. (active <$> bM2S)

    cM2S = mux bSelected bM2S aM2S
    ramOut = wbStorage' initContent cM2S

payloadContents :: FilePath -> IO (BitVector 32, [Byte], [Byte])
payloadContents path = do
  payloadData <- BL.readFile path
  let payloadBytes = pack <$> BL.unpack payloadData

  let payloadSize = fromIntegral $ BL.length payloadData :: BitVector 32
  let payloadSizeBytesLe =
        let (a :: Byte, b :: Byte, c :: Byte, d :: Byte) = unpack payloadSize
        in [d, c, b, a]
  
  let digest = md5 payloadData
  let md5Bytes = pack <$> BS.unpack (md5DigestBytes digest)
  let fullData = payloadSizeBytesLe <> md5Bytes <> payloadBytes

  pure (payloadSize, md5Bytes, fullData)


data UartReponseState
  = UWaitForNextChunk [Byte]
  | UTransmitChunk [Byte] [Byte]
  deriving (Generic, NFDataX)

uartResponse ::
  Int ->
  -- ^ Chunk size
  UartReponseState ->
  [Maybe Byte] ->
  -- ^ write
  [Maybe Byte]
uartResponse _         _                      []                   = L.repeat Nothing
uartResponse chunkSize (UWaitForNextChunk bs) (Nothing:inputs)     = uartResponse chunkSize (UWaitForNextChunk bs) inputs
uartResponse chunkSize (UWaitForNextChunk bs) (Just _:inputs)      = uartResponse chunkSize (UTransmitChunk chunk rest) inputs
  where
    (chunk, rest) = trace "split waiting" $ L.splitAt chunkSize bs
uartResponse _         (UTransmitChunk [] []) _                    = L.repeat Nothing
uartResponse chunkSize (UTransmitChunk [] bs) (Nothing:inputs)     = uartResponse chunkSize (UWaitForNextChunk bs) inputs
uartResponse chunkSize (UTransmitChunk [] bs) (Just _:inputs)      = uartResponse chunkSize (UTransmitChunk chunk rest) inputs
 where
    (chunk, rest) = trace "split with no bytes left" $ L.splitAt chunkSize bs
uartResponse chunkSize (UTransmitChunk (c:cs) bs) (Nothing:inputs) = -- trace ("transmit " <> showHex c "") $
  Just c : uartResponse chunkSize (UTransmitChunk cs bs) inputs
uartResponse chunkSize (UTransmitChunk cs bs)     (Just _:inputs)  = uartResponse chunkSize (UTransmitChunk chunk rest) inputs
  where
    (chunk, rest) = trace "split with bytes left" $ L.splitAt chunkSize (cs <> bs)

type Payload = [Byte]

transmitFn ::
  Int ->
  -- ^ chunk size
  Payload ->
  [Maybe Byte] ->
  ([Byte], Payload)
transmitFn chunkSize = go
  where
    go bs inputs
      | L.any isJust inputs = L.splitAt chunkSize bs
      | otherwise = ([], bs)


main :: IO ()
main = do
  let bootLoaderElf = "target/riscv32imc-unknown-none-elf/release/bootloader"

  (removeFiles, iMem, dMem) <-
    withClockResetEnable @System clockGen resetGen enableGen $
      loadProgram @System bootLoaderElf
  
  payloadPath <- L.head <$> getArgs
  (payloadSize, payloadMd5, payload) <- payloadContents payloadPath

  hSetBuffering stdin NoBuffering

  let baud = (SNat @6000000)

  let chunkSize = 128

  let
    (unbundle -> (bootUartRxBytes, bootUartRx)) = withClockResetEnable @System clockGen resetGen enableGen
      uartState baud payload (transmitFn chunkSize) bootUartTx
      -- runUart baud bootUartTx uartInp
    
    -- uartInp = uartResponse 128 (UWaitForNextChunk $ payload <> L.repeat 0) (sample bootUartRxBytes)

    cpuOut@(unbundle -> (_circuit, writes, _iBus, _dBus, _termUartTx, bootUartTx)) =
        withClockResetEnable @System clockGen (resetGenN (SNat @2)) enableGen $
          bundle (cpu iMem dMem baud (pure 1) bootUartRx)

  bracket (pure ()) (const removeFiles) $ \_ -> do
    case debugConfig of
      RunCharacterDevice -> do
        hSetBuffering stdin NoBuffering
        withClockResetEnable @System clockGen (resetGenN (SNat @2)) enableGen $
          uartIO stdin stdout baud (fmap (\(_,_,_,_,uartTx,_)-> uartTx) (\b -> cpu iMem dMem baud b bootUartRx))

      InspectBusses initCycles uninteresting interesting iEnabled dEnabled -> do
        
        let skipTotal = initCycles + uninteresting

        let sampled = case interesting of
              Nothing -> L.zip [0 ..] $ sample_lazy @System cpuOut
              Just nInteresting ->
                let total = initCycles + uninteresting + nInteresting in L.zip [0 ..] $ L.take total $ sample_lazy @System cpuOut

        forM_ sampled $ \(i, (out, _, iBusS2M, dBusS2M, _, _)) -> do
          let doPrint = i >= skipTotal

          -- I-bus interactions

          when (doPrint && iEnabled) $ do
            let iBusM2S = iBusWbM2S out
            let iAddr = toInteger (addr iBusM2S) -- `shiftL` 2

            let cyc = if busCycle iBusM2S then "CYC" else "   "
            let stb = if strobe iBusM2S then "STB" else "   "

            let iResp =
                  if
                      | acknowledge iBusS2M -> "ACK  "
                      | err iBusS2M -> "ERR  "
                      | retry iBusS2M -> "RETRY"
                      | otherwise -> "NONE "

            let iRespData =
                  if acknowledge iBusS2M
                    then printf "% 8X" (toInteger $ readData iBusS2M)
                    else "<no data>"

            putStr $
              "iM2S:   ("
                <> (cyc <> " " <> stb)
                <> ") "
                <> "("
                <> showX (busSelect iBusM2S)
                <> ") "
                <> printf "% 8X" iAddr
                <> " ("
                <> printf "%X" (iAddr `shiftL` 2)
                <> ")"
            putStrLn $ "            - iS2M: " <> iResp <> " - " <> iRespData

          -- D-bus interactions

          when (doPrint && dEnabled) $ do
            let dBusM2S = dBusWbM2S out
            let dAddr = toInteger (addr dBusM2S) -- `shiftL` 2
            let dWrite = writeEnable dBusM2S
            let cyc = if busCycle dBusM2S then "CYC" else "   "
            let stb = if strobe dBusM2S then "STB" else "   "
            let dValid = busCycle dBusM2S && strobe dBusM2S
            let dActive = busCycle dBusM2S

            let mode = if dWrite then "W" else "R"

            let dResp =
                  if
                      | acknowledge dBusS2M -> "ACK  "
                      | err dBusS2M -> "ERR  "
                      | retry dBusS2M -> "RETRY"
                      | otherwise -> "NONE "

            let dRespData
                  | acknowledge dBusS2M && hasUndefined (readData dBusS2M) && not dWrite = printf " - undefined!!"
                  | acknowledge dBusS2M && not dWrite = printf " - % 8X" (toInteger $ readData dBusS2M)
                  | not dWrite = " - <no data>"
                  | otherwise = ""

            let writeDat =
                  if dValid && dWrite
                    then printf "% 8X" (toInteger $ writeData dBusM2S)
                    else " no data"

            when (dActive || hasTerminateFlag dBusS2M) $ do
              putStr $
                "dM2S: "
                  <> mode
                  <> " ("
                  <> (cyc <> " " <> stb)
                  <> ") "
                  <> "("
                  <> showX (busSelect dBusM2S)
                  <> ") "
                  <> printf "% 8X" dAddr
                  <> " ("
                  <> printf "% 8X" (dAddr `shiftL` 2)
                  <> ") "
                  <> "<"
                  <> writeDat
                  <> "> - "
              putStrLn $ "dS2M: " <> dResp <> dRespData
      InspectWrites ->
        forM_ (catMaybes $ sample_lazy @System writes) $ \(addr, value) -> do
          printf "W: % 8X <- % 8X\n" (toInteger addr) (toInteger value)
