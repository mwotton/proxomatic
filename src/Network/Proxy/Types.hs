module Network.Proxy.Types where

import           Data.Attoparsec.ByteString (Parser)
import           Data.ByteString            (ByteString)

data Proxy = Proxy (Parser ByteString) (IO Bool)
type Port = Int
type Host = String
