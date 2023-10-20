-- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module VexRiscv.JtagTcpBridge where

import Clash.Prelude

import Clash.Signal.Internal
import Network.Socket
import Protocols
import Protocols.Internal (CSignal(..), Reverse)
import VexRiscv (JtagIn(..), JtagOut(..), Jtag)
import Control.Concurrent (MVar, forkIO, newEmptyMVar, putMVar, takeMVar, tryTakeMVar)
import Control.Monad (when)
import Network.Socket.ByteString (sendAll, recv)

import qualified Data.ByteString as BS
import System.IO.Unsafe (unsafePerformIO)
import Debug.Trace (trace)

data NetworkThreadToMainMsg
  = Connected
  | Disconnected
  | DataReceived [BitVector 8]

data MainToNetworkThreadMsg
  = ReadMore
  | Send (BitVector 8)

data NetworkThreadState
  = NoClient
  | PerformRead Socket
  | WaitForNextRead Socket
  deriving (Show)

data MainThreadState
  = MDisconnected
  | MWaitForRead
  | MProcessing (BitVector 2) [BitVector 8]
  deriving (Show)

jtagTcpBridge ::
  (HiddenClockResetEnable dom) =>
  PortNumber ->
  Circuit
    (Jtag dom, Reverse (CSignal dom Bool))
    ()
jtagTcpBridge port = 
  Circuit $ \((jtagOut, _), ()) -> unsafePerformIO $ do
    (enable, jtagIn) <- jtagTcpBridge' port jtagOut
    pure ((jtagIn, CSignal $ fromEnable enable), ())

jtagTcpBridge' ::
  (KnownDomain dom) =>
  PortNumber ->
  Signal dom JtagOut ->
  IO (Enable dom, Signal dom JtagIn)
jtagTcpBridge' port jtagOut = do

  (n2m, m2n) <- server port
  
  (unbundle -> (enable, jtagIn)) <- client n2m m2n MDisconnected jtagOut

  pure (toEnable enable, jtagIn)

{-# NOINLINE jtagTcpBridge' #-}

server :: PortNumber -> IO (MVar NetworkThreadToMainMsg, MVar MainToNetworkThreadMsg)
server port = withSocketsDo $ do
  sock <- setup
  
  threadToMainChan <- newEmptyMVar
  mainToThreadChan <- newEmptyMVar
    
  let
      thread NoClient = do
        (clientSock, _) <- accept sock
        putMVar threadToMainChan Connected
        thread (PerformRead clientSock)
      thread (PerformRead clientSock) = do
        buf <- recv clientSock 100
        if BS.null buf then do
          putMVar threadToMainChan Disconnected
          thread NoClient
        else do
          let dat = pack <$> BS.unpack buf
          putMVar threadToMainChan (DataReceived dat)
          thread (WaitForNextRead clientSock)
      
      thread (WaitForNextRead clientSock) = do
        msg <- takeMVar mainToThreadChan
        case msg of
          ReadMore -> thread (PerformRead clientSock)
          Send byte -> do
            sendAll clientSock (BS.singleton $ unpack byte)
            thread (WaitForNextRead clientSock)

  _ <- forkIO $ thread NoClient
      
  pure (threadToMainChan, mainToThreadChan)

  where
    setup = do
      sock <- socket AF_INET Stream 0

      setSocketOption sock NoDelay 0

      bind sock (SockAddrInet port (tupleToHostAddress (127, 0, 0, 1)))

      listen sock 1

      pure sock

defaultIn :: JtagIn
defaultIn = JtagIn { testModeSelect = low, testDataIn = low }

dbg :: Show a => a -> a
dbg x = 
  trace (show x)
  x

clientSleep :: BitVector 2
clientSleep = 4

client ::
  (KnownDomain dom) =>
  MVar NetworkThreadToMainMsg ->
  MVar MainToNetworkThreadMsg ->
  MainThreadState ->
  Signal dom JtagOut ->
  IO (Signal dom (Bool, JtagIn))
client n2m m2n MDisconnected (_out :- outs) = do
  msg <- tryTakeMVar n2m
  case msg of
    Nothing ->
      pure $ _out `deepseqX` (False, defaultIn) :- unsafePerformIO (client n2m m2n MDisconnected outs)
    Just Connected -> do
      pure $ _out `deepseqX` (False, defaultIn) :- unsafePerformIO (client n2m m2n MWaitForRead outs)
    Just Disconnected -> do
      errorX "????"
    Just (DataReceived _xs) -> do
      errorX "????"
  
client n2m m2n MWaitForRead (out :- outs) = do
  msg <- tryTakeMVar n2m
  case msg of
    Nothing ->
      pure $ out `deepseqX` (False, defaultIn) :- unsafePerformIO (client n2m m2n MWaitForRead outs)
    Just Disconnected ->
      pure $ out `deepseqX` (False, defaultIn) :- unsafePerformIO (client n2m m2n MDisconnected outs)
    Just (DataReceived xs) ->
      client n2m m2n (MProcessing 0 xs) (out :- outs)
    Just Connected ->
      errorX "????"

client n2m m2n (MProcessing _ []) (out :- outs) = do
  putMVar m2n ReadMore
  pure $ out `deepseqX` (False, defaultIn) :- unsafePerformIO (client n2m m2n MWaitForRead outs)
client n2m m2n (MProcessing 0 (x:xs)) (out :- outs) = do
  let tms = x ! (0 :: Int)
      tdi = x ! (1 :: Int)
      tck = x ! (3 :: Int)

      sendTdo = bitToBool $ x ! (2 :: Int)

      enable = bitToBool tck
  
  when sendTdo $ do
    let tdo = bitToBool $ testDataOut out
    -- putStrLn $ "send TDO " <> show tdo
    putMVar m2n $ Send $ boolToBV tdo

  let inDat = JtagIn { testModeSelect = tms, testDataIn = tdi }

  when enable $ do
    -- putStrLn "Enable"
    -- putStrLn $ "IN " <> showX inDat
    pure ()

  pure $ (enable, inDat) :- unsafePerformIO (client n2m m2n (MProcessing clientSleep xs) outs)
client n2m m2n (MProcessing n xs) (out :- outs) = do
  pure $ out `deepseqX` (False, defaultIn) :- unsafePerformIO (client n2m m2n (MProcessing (n - 1) xs) outs)


{-# NOINLINE client #-}