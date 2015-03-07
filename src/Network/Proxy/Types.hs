module Network.Proxy.Types where

import           Data.Attoparsec.ByteString (Parser)
import           Data.ByteString            (ByteString)

type Proxy = Parser ByteString
type Port = Int
type Host = ByteString
