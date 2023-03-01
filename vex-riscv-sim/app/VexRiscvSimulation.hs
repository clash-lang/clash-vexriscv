-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE ApplicativeDo #-}
-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}

import Bittide.DoubleBufferedRam (ContentType (..), InitialContent (..), wbStorage', wbStorageDP)
import Bittide.SharedTypes hiding (delayControls)
import Bittide.Wishbone (singleMasterInterconnect')
import Clash.Explicit.BlockRam.File (memFile)
import Clash.Prelude hiding (not, (&&))
import Clash.Signal.Internal (Signal ((:-)))
import Control.Exception (bracket)
import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.IntMap as I
import qualified Data.List as L
import Data.Maybe (catMaybes)
import GHC.Base (assert)
import GHC.IO.Handle
import GHC.Stack (HasCallStack)
import qualified GHC.TypeNats as TN
import Protocols.Wishbone
import System.Directory (removeFile)
import System.Environment
import System.IO (openTempFile)
import Text.Printf
import Utils.Print (performPrintsToStdout)
import Utils.ReadElf
import VexRiscv
import Prelude as P hiding ((||))

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
    Int ->
    -- | inspect instruct-bus interactions
    Bool ->
    -- | inspect data-bus interactions
    Bool ->
    DebugConfiguration
  -- | Run a program and output all the write operations
  InspectWrites :: DebugConfiguration

-- change this variable to the configuration you want to use

debugConfig :: DebugConfiguration
debugConfig = RunCharacterDevice

{-
InspectBusses
  100
  0
  500
  True
  True
-}

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
Address space

0b0000 0x0000_0000 Character device / Debug addresses
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
  (HasCallStack, HiddenClockResetEnable dom) =>
  Memory dom ->
  Memory dom ->
  ( Signal dom Output,
    -- writes
    Signal dom (Maybe (BitVector 32, BitVector 32)),
    -- iBus responses
    Signal dom (WishboneS2M (BitVector 32)),
    -- dBus responses
    Signal dom (WishboneS2M (BitVector 32))
  )
cpu (_iMemStart, bootIMem) (_dMemStart, bootDMem) = (output, writes, iS2M, dS2M)
  where
    output = vexRiscv (emptyInput :- input)
    dM2S = dBusWbM2S <$> output

    errS2M = emptyWishboneS2M {err = True}

    (iS2M, unbundle -> (_ :> bootIM2S :> _ :> loadedIM2S :> _ :> Nil)) =
      singleMasterInterconnect'
        -- 3 bit prefix
        (0b000 :> 0b001 :> 0b010 :> 0b011 :> 0b100 :> Nil)
        (unBusAddr . iBusWbM2S <$> output)
        (bundle (pure errS2M :> bootIS2M :> pure errS2M :> loadedIS2M :> pure errS2M :> Nil))

    bootIS2M = bootIMem (mapAddr @29 @32 resize <$> bootIM2S)
    (loadedIS2M, loadedIS2MDbus) =
      wbStorageDP
        (Undefined :: InitialContent 4096 (Bytes 4))
        -- port A, prioritised, instruction bus
        (mapAddr @29 @32 resize <$> loadedIM2S)
        -- port B, data bus
        (mapAddr @29 @32 resize <$> loadedIM2SDbus)

    -- iWb = iMem (mapAddr (\x -> x - iMemStart) . unBusAddr . iBusWbM2S <$> output)

    (_, dummy) = dummyWb 0x0000_0000

    dummyS2M = dummy (mapAddr @29 @32 resize <$> dummyM2S)
    bootDS2M = bootDMem (mapAddr @29 @32 resize <$> bootDM2S)
    loadedDS2M = wbStorage' (Undefined :: InitialContent 4096 (Bytes 4)) (mapAddr @29 @32 resize <$> loadedDM2S)

    (dS2M, unbundle -> (dummyM2S :> _ :> bootDM2S :> loadedIM2SDbus :> loadedDM2S :> Nil)) =
      singleMasterInterconnect'
        -- 3 bit prefix
        (0b000 :> 0b001 :> 0b010 :> 0b011 :> 0b100 :> Nil)
        -- going from 0b0xyz to 0bxyz (32bit to 31 bit)
        (unBusAddr . dBusWbM2S <$> output)
        ( bundle
            (dummyS2M :> pure errS2M :> bootDS2M :> loadedIS2MDbus :> loadedDS2M :> Nil)
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

-- | Wishbone circuit that always acknowledges every request
--
-- Used for the character device. The character device address gets mapped to this
-- component because if it were to be routed to the data memory (where this address is
-- not in the valid space) it would return ERR and would halt execution.
dummyWb :: (HiddenClockResetEnable dom) => BitVector 32 -> Memory dom
dummyWb address = (address, \m2s -> delayControls m2s (reply <$> m2s))
  where
    reply WishboneM2S {..} =
      (emptyWishboneS2M @(BitVector 32)) {acknowledge = acknowledge, readData = 0}
      where
        acknowledge = busCycle && strobe

    -- \| Delays the output controls to align them with the actual read / write timing.
    delayControls ::
      (HiddenClockResetEnable dom, NFDataX a) =>
      Signal dom (WishboneM2S addressWidth selWidth a) -> -- current M2S signal
      Signal dom (WishboneS2M a) ->
      Signal dom (WishboneS2M a)
    delayControls m2s s2m0 = mux inCycle s2m1 (pure emptyWishboneS2M)
      where
        inCycle = (busCycle <$> m2s) .&&. (strobe <$> m2s)

        -- It takes a single cycle to lookup elements in a block ram. We can therfore
        -- only process a request every other clock cycle.
        ack = (acknowledge <$> s2m0) .&&. (not <$> delayedAck) .&&. inCycle
        err1 = (err <$> s2m0) .&&. inCycle
        delayedAck = register False ack
        delayedErr1 = register False err1
        s2m1 =
          (\wb newAck newErr -> wb {acknowledge = newAck, err = newErr})
            <$> s2m0
            <*> delayedAck
            <*> delayedErr1

loadProgram :: (HiddenClockResetEnable dom) => FilePath -> IO (IO (), Memory dom, Memory dom)
loadProgram path = do
  elfBytes <- BS.readFile path
  let (entry, iMem, dMem) = readElfFromMemory elfBytes

  assert (entry == 0x2000_0000) (pure ())

  (iPath, iHandle) <- openTempFile "/tmp" "imem.blob"

  (d0Path, d0Handle) <- openTempFile "/tmp" "dmem.0.blob"
  (d1Path, d1Handle) <- openTempFile "/tmp" "dmem.1.blob"
  (d2Path, d2Handle) <- openTempFile "/tmp" "dmem.2.blob"
  (d3Path, d3Handle) <- openTempFile "/tmp" "dmem.3.blob"

  let removeFiles = mapM_ removeFile [iPath, d0Path, d1Path, d2Path, d3Path]

  let -- endian swap instructions
      iMemBS =
        memFile Nothing $
          L.map (\[a, b, c, d] -> bitCoerce (d, c, b, a) :: BitVector 32) $
            chunkFill 4 0 (content iMem)

      (dL0, dL1, dL2, dL3) = split4 $ content dMem
      dMem0BS = memFile Nothing dL0
      dMem1BS = memFile Nothing dL1
      dMem2BS = memFile Nothing dL2
      dMem3BS = memFile Nothing dL3

      iMemStart = startAddr iMem
      dMemStart = startAddr dMem

      iMemSize = I.size iMem `divRU` 4
      dMemSize = I.size dMem `divRU` 4

      dContentVec = d0Path :> d1Path :> d2Path :> d3Path :> Nil

  assert (dMemStart == 0x4000_0000) (pure ())

  -- write data to files
  hPutStr iHandle iMemBS
  -- endian swap data
  hPutStr d0Handle dMem3BS
  hPutStr d1Handle dMem2BS
  hPutStr d2Handle dMem1BS
  hPutStr d3Handle dMem0BS

  -- close files
  hClose iHandle
  hClose d0Handle
  hClose d1Handle
  hClose d2Handle
  hClose d3Handle

  let instrMem = case TN.someNatVal (toEnum iMemSize) of
        SomeNat (snatProxy -> depth) ->
          case compareSNat depth d1 of
            SNatLE -> error "should not happen"
            SNatGT ->
              let initContent = helper depth $ Reloadable $ File iPath
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

main :: IO ()
main = do
  elfFile <- L.head <$> getArgs

  (removeFiles, iMem, dMem) <-
    withClockResetEnable @System clockGen resetGen enableGen $
      loadProgram @System elfFile

  let cpuOut@(unbundle -> (_circuit, writes, _iBus, _dBus)) =
        withClockResetEnable @System clockGen (resetGenN (SNat @2)) enableGen $
          bundle (cpu iMem dMem)

  bracket (pure ()) (const removeFiles) $ \_ -> do
    case debugConfig of
      RunCharacterDevice ->
        performPrintsToStdout 0x0000_1000 (sample_lazy $ bitCoerce <$> writes)
      InspectBusses initCycles uninteresting interesting iEnabled dEnabled -> do
        let total = initCycles + uninteresting + interesting
        let skipTotal = initCycles + uninteresting

        let sampled = L.zip [0 ..] $ L.take total $ sample_lazy @System cpuOut

        forM_ sampled $ \(i, (out, _, iBusS2M, dBusS2M)) -> do
          let doPrint = i >= skipTotal

          -- I-bus interactions

          when (doPrint && iEnabled) $ do
            let iBus = iBusWbM2S out
            let iAddr = toInteger (addr iBus) -- `shiftL` 2
            let iValid = busCycle iBus && strobe iBus
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
                <> (if not iValid then "!" else " ")
                <> "V) "
                <> "("
                <> showX (busSelect iBus)
                <> ") "
                <> printf "% 8X" iAddr
                <> " ("
                <> printf "%X" (iAddr `shiftL` 2)
                <> ")"
            putStrLn $ "            - iS2M: " <> iResp <> " - " <> iRespData

          -- D-bus interactions

          when (doPrint && dEnabled) $ do
            let dBus = dBusWbM2S out
            let dAddr = toInteger (addr dBus) -- `shiftL` 2
            let dWrite = writeEnable dBus
            let dValid = busCycle dBus && strobe dBus

            let mode = if dWrite then "W" else "R"

            let dResp =
                  if
                      | acknowledge dBusS2M -> "ACK  "
                      | err dBusS2M -> "ERR  "
                      | retry dBusS2M -> "RETRY"
                      | otherwise -> "NONE "

            let dRespData
                  | acknowledge dBusS2M = printf " - % 8X" (toInteger $ readData dBusS2M)
                  | not dWrite = " - <no data>"
                  | otherwise = ""

            let writeDat =
                  if dValid && dWrite
                    then printf "% 8X" (toInteger $ writeData dBus)
                    else " no data"

            when (dValid || hasTerminateFlag dBusS2M) $ do
              putStr $
                "dM2S: "
                  <> mode
                  <> " ("
                  <> (if not dValid then "!" else " ")
                  <> "V) "
                  <> "("
                  <> showX (busSelect dBus)
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
