{-# LANGUAGE OverloadedStrings #-}
module Network.Proxy.Redis where

import           Control.Applicative              ((<$>), (<*))
import           Data.Attoparsec.ByteString.Char8 (anyChar, manyTill, string)
import qualified Data.ByteString.Char8            as BS
import           Data.Monoid                      ((<>))
import           Network.Proxy.Types
import           Prelude

proxy :: [(Host, Port)] -> Proxy (Host,Port)
proxy ports = Proxy parser (return (Just (head ports)))

parser = (<>"\r\n") . BS.pack <$>
         manyTill anyChar (string "\r\n") <* string "\r\n"
