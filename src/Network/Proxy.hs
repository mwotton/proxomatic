{-# LANGUAGE OverloadedStrings, TupleSections #-}
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
  unblocker <- newIORef (Just [])
  canary <- async $ forever $ runHealthCheck unblocker
  serve (Host remote) (show listenPort) (handler unblocker)

  where
    handler unblocker (listenSock, remoteAddr) = do
      putStrLn $ "TCP connection established from " <> tshow remoteAddr
      blockIfNecessary unblocker

      result <- liftIO $ netParse parser listenSock
      case result of
        Done _ command -> do
          connect remote (show connectPort) $ \(connectSock, _) -> do
            send connectSock command
            untilDone (recv connectSock 65536) (send listenSock)
        x -> print ("bad shit happened parsing", x)


    blockIfNecessary unblocker = do
      -- strictly speaking we don't need to have this outer layer.
      -- however, we would be creating a new & mostly unused mvar
      -- every time through the loop if we just used atomicModifyIORef'
      -- so we check it first, and if it has nothing waiting (normal
      -- case) we can proceed without further fuss.

      ready <- readIORef unblocker
      case ready of
        Nothing -> return () -- all good, we aren't waiting on anything.
        Just _ -> do
          -- stuff to do, create a new mvar & wait on it
          blocker <- newEmptyMVar
          -- this adds the mvar to the list iff we are still blocked.
          join $ atomicModifyIORef' unblocker (addToBlock blocker)

    addToBlock blocker Nothing =  (Nothing, return ())
    addToBlock blocker (Just blocked) = (Just (blocker:blocked), takeMVar blocker)

    runHealthCheck unblocker = do
      healthCheck >>= \x ->
        if x
        then do
          atomicModifyIORef' unblocker ((Nothing,) . fromMaybe [] )
          >>= mapM_ (`putMVar` ())
        else atomicModifyIORef' unblocker
           (\r -> (Just (fromMaybe [] r), ()))
      threadDelay 1000000

untilDone from to = do
  r <- from
  case r of
    Nothing -> return ()
    Just x -> to x >> untilDone from to

netParse p sock = parseWith (fromMaybe "" <$> recv sock 65536) p BS.empty
