{-# LANGUAGE OverloadedStrings #-}
module Network.Proxy.Redis where

import           Control.Applicative              ((<$>), (<*))
import           Control.Concurrent.Async         (mapConcurrently)
import           Control.Monad.Trans              (liftIO)
import           Control.Monad.Trans.State
import           Data.Attoparsec.ByteString       (Parser)
import           Data.Attoparsec.ByteString.Char8 (anyChar, manyTill, string)
import qualified Data.ByteString.Char8            as BS
import qualified Data.HashMap.Strict              as HM
import           Data.Maybe                       (fromMaybe)
import           Data.Monoid                      ((<>))
import           Network.Proxy.Types
import           Network.Simple.TCP               (connectSock, recv, send)
import           Network.Simple.TCP               (Socket)
import           Network.SocketUtil               (netParse)

-- TODO add more reasonable healthcheck
-- proxy :: [(Host, Port)] -> Proxy (Host,Port)

proxy :: [(String, Int)] -> Proxy (Host, Port) (HM.HashMap (Host, Port) Socket)
proxy ports = Proxy parser (HM.empty, healthCheck ports)


parser :: Parser BS.ByteString
parser = (<>"\r\n") . BS.pack <$>
         manyTill anyChar (string "\r\n") <* string "\r\n"


healthCheck :: [(String,Int)] -> StateT (HM.HashMap (Host,Port) Socket) IO (Maybe (Host,Port))
healthCheck ports = do
  openConns <- get

  statuses <- liftIO $ (`mapConcurrently` ports) $ \hp@(host,port) -> do
    conn <- case HM.lookup hp openConns of
      Nothing -> print ("connecting", host, port) >> fst <$> connectSock host (show port)
      Just x -> return x
    send conn "role\n"
    result <- netParse (BS.pack <$> manyTill anyChar (string "*0\r\n")) conn
    let isMaster = case BS.lines <$> result of
          Just (_:_:x:_) -> x == "master\r"
          _ -> False
    return (isMaster,(hp,conn))
  put $ HM.fromList $ map snd statuses
  return $ case filter fst statuses of
    -- one master, everything is ok.
    [(_,(hp,_))] -> Just hp
    _ -> Nothing
