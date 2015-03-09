module Network.Proxy.Types where

import           Data.Attoparsec.ByteString (Parser)
import           Data.ByteString            (ByteString)

data Proxy a = Proxy (Parser ByteString) (IO (Maybe a))
type Port = Int
type Host = String
