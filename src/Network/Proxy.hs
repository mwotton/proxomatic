{-# LANGUAGE OverloadedStrings #-}
module Network.Proxy where

import           Control.Applicative        ((<$>))
import           Control.Concurrent         (threadDelay)
import           Control.Concurrent.Async   (async)
import           Control.Monad              (forever)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Trans.State  (runStateT)
import           Data.Attoparsec.ByteString (IResult (..), Parser, Result,
                                             parseWith)
import           Data.Blocker               (newBlocker)
import qualified Data.ByteString            as BS
import           Data.Maybe                 (fromMaybe)
import           Data.Monoid                ((<>))
import           Network.Proxy.Types
import           Network.Simple.TCP         (HostName, HostPreference (..),
                                             Socket, connect, recv, send, serve)



-- proxy :: (Show a1) => a1 -> Proxy b (Host, Port) -> IO ()
proxy :: (Show a1, Show a) => a -> Proxy (HostName, a1) s -> IO ()
proxy listenPort (Proxy parser (initial, healthCheck)) = do
  (blockIfNecessary, block, unblock) <- newBlocker

  _canary <- async $ (`runStateT` initial) $
             forever $ do
               healthCheck >>= liftIO . maybe block unblock
               liftIO (threadDelay 1000000)

  serve (Host "127.0.0.1") (show listenPort) (handler blockIfNecessary)

  where
    handler blockIfNecessary (listenSock, remoteAddr) = do

      -- TODO add loop & check for new host:port pair so we can
      -- keep persistent connections up
      putStrLn $ "TCP connection established from " <> show remoteAddr
      (connectHost,connectPort) <- blockIfNecessary

      result <- liftIO $ netParse parser listenSock
      case result of
        Done _ command -> do
          connect connectHost (show connectPort) $ \(connectSock, _) -> do
            send connectSock command
            untilDone (recv connectSock 65536) (send listenSock)
        -- TODO make this suck less
        x -> print ("error during parsing",x)

untilDone :: Monad m => m (Maybe a) -> (a -> m a1) -> m ()
untilDone from to = from >>= maybe (return ())
                                   (\x -> to x >> untilDone from to)

netParse :: (MonadIO m, Functor m) => Parser a -> Socket -> m (Result a)
netParse p sock = parseWith (fromMaybe "" <$> recv sock 65536) p BS.empty
