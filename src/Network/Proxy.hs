{-# LANGUAGE OverloadedStrings #-}
module Network.Proxy where
import           ClassyPrelude
import           Control.Concurrent         (threadDelay)
import           Control.Concurrent.Async   (async)
import           Data.Attoparsec.ByteString (IResult (..), parseWith)
import qualified Data.ByteString            as BS
import           Network.Proxy.Types
import           Network.Simple.TCP         (HostPreference (..), connect, recv,
                                             send, serve)
import           Prelude                    ()

proxy (listenPort, (remote, connectPort), Proxy parser healthCheck) = do
  unblocker <- newEmptyMVar
  canary <- async $ forever $ do
              healthCheck >>= \x ->
                if x
                then void $ tryPutMVar unblocker ()
                else void $ tryTakeMVar unblocker
              threadDelay 1000000
  serve (Host remote) (show listenPort) $ \(listenSock, remoteAddr) -> do
    putStrLn $ "TCP connection established from " <> tshow remoteAddr
    takeMVar unblocker
    putMVar unblocker ()
    result <- liftIO $ netParse parser listenSock
    case result of
      Done _ command -> do
        connect remote (show connectPort) $ \(connectSock, _) -> do
          send connectSock command
          untilDone (recv connectSock 65536) (send listenSock)

      x -> do
        print ("bad shit happened parsing", x)

untilDone from to = do
  r <- from
  case r of
    Nothing -> return ()
    Just x -> to x >> untilDone from to


netParse p sock = parseWith (fromMaybe "" <$> recv sock 65536) p BS.empty
