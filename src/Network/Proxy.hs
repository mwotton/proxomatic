{-# LANGUAGE OverloadedStrings, TupleSections #-}
module Network.Proxy where

import           Control.Applicative        ((<$>))
import           Control.Concurrent         (threadDelay)
import           Control.Concurrent.Async   (async)
import           Control.Monad              (forever)
import           Control.Monad.IO.Class     (liftIO)
import           Data.Attoparsec.ByteString (IResult (..), parseWith)
import qualified Data.ByteString            as BS

import           Data.Blocker               (newBlocker)
import           Data.Maybe                 (fromMaybe)
import           Data.Monoid                ((<>))
import           Network.Proxy.Types
import           Network.Simple.TCP         (HostPreference (..), connect, recv,
                                             send, serve)

proxy (listenPort, (remote, connectPort), Proxy parser healthCheck) = do
  (blockIfNecessary, block, unblock) <- newBlocker

  canary <- async $ forever $ runHealthCheck block unblock
  serve (Host remote) (show listenPort) (handler blockIfNecessary)

  where
    handler blockIfNecessary (listenSock, remoteAddr) = do
      putStrLn $ "TCP connection established from " <> show remoteAddr
      blockIfNecessary

      result <- liftIO $ netParse parser listenSock
      case result of
        Done _ command -> do
          connect remote (show connectPort) $ \(connectSock, _) -> do
            send connectSock command
            untilDone (recv connectSock 65536) (send listenSock)
        x -> print ("bad shit happened parsing", x)



    runHealthCheck block unblock = do
      healthCheck >>= \ok ->
        if ok
        then unblock
        else block
      threadDelay 1000000

untilDone from to = do
  r <- from
  case r of
    Nothing -> return ()
    Just x -> to x >> untilDone from to

netParse p sock = parseWith (fromMaybe "" <$> recv sock 65536) p BS.empty
