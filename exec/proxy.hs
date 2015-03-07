{-# LANGUAGE OverloadedStrings #-}
import           Network.Proxy
import qualified Network.Proxy.Redis as Redis

main :: IO ()
main = proxy [(8111, ("localhost", 9999), Redis.proxy)]
