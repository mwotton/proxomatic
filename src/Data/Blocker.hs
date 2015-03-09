{-# LANGUAGE TupleSections #-}
module Data.Blocker where
import           Control.Concurrent (newEmptyMVar, putMVar, takeMVar,
                                     threadDelay)
import           Data.IORef         (atomicModifyIORef', newIORef, readIORef)
import           Data.Maybe         (fromMaybe)


newBlocker :: IO (IO (), IO (), IO ())
newBlocker = do
  ref <- newIORef (Just [])
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
        Nothing -> return () -- all good, we aren't waiting on anything.
        Just _ -> do
          -- stuff to do, create a new mvar & wait on it
          blocker <- newEmptyMVar
          -- this adds the mvar to the list iff we are still blocked.
          todo <- atomicModifyIORef' ref (addToBlock blocker)
          todo

    addToBlock blocker Nothing =  (Nothing, return ())
    addToBlock blocker (Just blocked) = (Just (blocker:blocked), takeMVar blocker)

    block ref  = atomicModifyIORef' ref ((Nothing,) . fromMaybe [] ) >>= mapM_ (`putMVar` ())
    unblock ref =  atomicModifyIORef' ref     (\r -> (Just (fromMaybe [] r), ()))
