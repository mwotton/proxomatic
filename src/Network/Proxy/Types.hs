module Network.Proxy.Types where

import           Control.Monad.Trans.State  (StateT)
import           Data.Attoparsec.ByteString (Parser)
import           Data.ByteString            (ByteString)

data Proxy a b = Proxy (Parser ByteString) (b, StateT b IO (Maybe a))
type Port = Int
type Host = String
