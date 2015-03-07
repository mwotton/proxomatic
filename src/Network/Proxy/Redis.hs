{-# LANGUAGE OverloadedStrings #-}
module Network.Proxy.Redis where
import           ClassyPrelude
import           Control.Applicative              ((<$>))
import           Data.Attoparsec.ByteString.Char8 (anyChar, manyTill, string)
import qualified Data.ByteString.Char8            as BS
import           Network.Proxy.Types
import           Prelude                          ()

proxy :: [(Host, Port)] -> Proxy
proxy ports = Proxy parser (return True)

parser = (<>"\r\n") . BS.pack <$>
         manyTill anyChar (string "\r\n") <* string "\r\n"
