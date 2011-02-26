import Prelude hiding ( lookup )

import Control.Monad
import Control.Concurrent
import Data.IORef

import Data.Cache.LRU.IO.Ordered

main :: IO ()
main = do
  v1 <- newAtomicLRU $ Just 10 -- for endless inserts
  v2 <- newAtomicLRU $ Just 10 -- for endless lookups (miss)
  v3 <- newAtomicLRU $ Just 10 -- for endless lookups (hit)

  counter <- newIORef (0 :: Int)

  insert 1 "bar" v3

  forever $ do
         c <- readIORef counter
         writeIORef counter $ c + 1

         insert c (show c) v1
         lookup (1 :: Int) v2
         lookup (1 :: Int) v3
