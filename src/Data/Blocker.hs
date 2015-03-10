{-# LANGUAGE TupleSections #-}
module Data.Blocker where
import           Control.Concurrent (newEmptyMVar, putMVar, takeMVar,
                                     threadDelay)
import           Control.Monad      (join)
import           Data.IORef         (atomicModifyIORef', newIORef, readIORef)
import           Data.Maybe         (fromMaybe)

newBlocker :: IO (IO a, IO (), a -> IO ())
newBlocker = do
  ref <- newIORef (Left [])
  return (blockIfNecessary ref, block ref, unblock ref)

  where
    blockIfNecessary ref = do
          -- strictly speaking we don't need to have this outer layer.
          -- however, we would be creating a new & mostly unused mvar
          -- every time through the loop if we just used atomicModifyIORef'
          -- so we check it first, and if it has nothing waiting (normal
          -- case) we can proceed without further fuss.

      ready <- readIORef ref
      case ready of
        Right x -> return x -- all good, we aren't waiting on anything.
        Left _ -> do
          -- stuff to do, create a new mvar & wait on it
          blocker <- newEmptyMVar
          -- this adds the mvar to the list iff we are still blocked.
          todo <- atomicModifyIORef' ref (addToBlock blocker)
          todo

    addToBlock blocker (Right details) = (Right details, return details)
    addToBlock blocker (Left blocked)  = (Left (blocker:blocked), takeMVar blocker)

    unblock ref val =
      join $ atomicModifyIORef' ref
        (either ((Right val,) . mapM_ (`putMVar` val))
                (const (Right val,return ())))

    block ref  =
      atomicModifyIORef' ref
        (either (\x -> (Left x,  ()))
                (const (Left [],  ())))
