-- | Convert an ELF file to a set of @.mem@ files, suitable for use with
-- Verilog simulators. Use in combination with @readmemh@ in Verilog.
module Main where

import Prelude

import Control.Concurrent.Async (mapConcurrently_)
import Control.Monad (forM_)
import Data.IntMap (IntMap, findWithDefault)
import Data.Tuple.Extra (uncurry3)
import System.Environment (getArgs)
import System.IO (withFile, IOMode(WriteMode), hPutStrLn)
import Text.Printf (printf)
import Text.Read (readMaybe)
import Utils.ReadElf (readElfFromMemory)

import qualified Data.ByteString as BS
import qualified Clash.Prelude as C

-- | Like 'zipWithM_', but execute concurrently
zipWithConcurrently_ :: (a -> b -> IO ()) -> [a] -> [b] -> IO ()
zipWithConcurrently_ f xs ys = mapConcurrently_ (uncurry f) (zip xs ys)

-- | Like 'zipWith3M_', but execute concurrently
zipWith3Concurrently_ :: (a -> b -> c -> IO ()) -> [a] -> [b] -> [c] -> IO ()
zipWith3Concurrently_ f xs ys zs = mapConcurrently_ (uncurry3 f) (zip3 xs ys zs)

-- | Convenience function to get data from a memory map. If a memory address is
-- not found, return 0.
getData :: Num a => IntMap a -> Int -> a
getData mem addr = findWithDefault 0 addr mem

-- | Generate the addresses for the four memory banks, given the total size in
-- bytes, and the start address.
getAddrs :: Int -> Int -> ([Int], [Int], [Int], [Int])
getAddrs size start =
  ( [start + 0, start + 4 .. start + size - 1]
  , [start + 1, start + 5 .. start + size - 1]
  , [start + 2, start + 6 .. start + size - 1]
  , [start + 3, start + 7 .. start + size - 1] )

-- | Write a single @.mem@ file
writeByteMem :: IntMap (C.BitVector 8) -> FilePath -> [Int] -> IO ()
writeByteMem mem path addrs = do
  putStrLn $ "Writing " <> path
  withFile path WriteMode $ \h ->
    forM_ addrs $ \addr -> do
      hPutStrLn h (toHex (getData mem addr))

-- | Write four @.mem@ files
writeMem :: Int -> IntMap (C.BitVector 8) -> FilePath -> Int -> IO ()
writeMem size mem prefix start = do
  let (addrs0, addrs1, addrs2, addrs3) = getAddrs size start

  zipWithConcurrently_
    (writeByteMem mem)
    [prefix <> show n <> ".mem" | n <- [(0::Int)..]]
    [addrs0, addrs1, addrs2, addrs3]

-- | Print a byte as a hex string of the form 0x00.
toHex :: C.BitVector 8 -> String
toHex = printf "0x%02x" . toInteger

main :: IO ()
main = do
  [sizeStr, elfFile] <- getArgs
  let Just size = readMaybe @Int sizeStr
  (_addr, iMem, dMem) <- readElfFromMemory <$> BS.readFile elfFile

  zipWith3Concurrently_
    (writeMem size)
    [iMem, dMem]
    ["imem", "dmem"]
    [0x20000000, 0x40000000]
