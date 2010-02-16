{-# OPTIONS_HADDOCK not-home #-}

-- | This module contains a mutable wrapping of an LRU in the IO
-- monad, providing atomic access in a concurrent environment.  All
-- calls preserve the same semantics as those in "Data.Cache.LRU", but
-- perform updates in place.
--
-- This module contains the internal implementation details.  It's
-- possible to put an 'AtomicLRU' into a bad state with this module.
-- It is highly recommended that the external interface,
-- "Data.Cache.LRU.IO", be used instead.
module Data.Cache.LRU.IO.Internal where

import Prelude hiding ( lookup )

import Control.Applicative ( (<$>) )
import Control.Concurrent.MVar ( MVar )
import qualified Control.Concurrent.MVar as MV

import Data.Cache.LRU ( LRU )
import qualified Data.Cache.LRU as LRU

-- | The opaque wrapper type
newtype AtomicLRU key val = C (MVar (LRU key val))

-- | Make a new AtomicLRU with the given maximum size.
newAtomicLRU :: Ord key => Int -- ^ the maximum size
             -> IO (AtomicLRU key val)
newAtomicLRU = fmap C . MV.newMVar . LRU.newLRU

-- | Build a new LRU from the given maximum size and list of
-- contents. See 'LRU.fromList' for the semantics.
fromList :: Ord key => Int -- ^ the maximum size
            -> [(key, val)] -> IO (AtomicLRU key val)
fromList s l = fmap C . MV.newMVar $ LRU.fromList s l

-- | Retreive a list view of an AtomicLRU.  See 'LRU.toList' for the
-- semantics.
toList :: Ord key => AtomicLRU key val -> IO [(key, val)]
toList (C mvar) = LRU.toList <$> MV.readMVar mvar

maxSize :: AtomicLRU key val -> IO Int
maxSize (C mvar) = LRU.maxSize <$> MV.readMVar mvar

-- | Insert a key/value pair into an AtomicLRU.  See 'LRU.insert' for
-- the semantics.
insert :: Ord key => key -> val -> AtomicLRU key val -> IO ()
insert key val (C mvar) = modifyMVar_' mvar $ return . LRU.insert key val

-- | Look up a key in an AtomicLRU. See 'LRU.lookup' for the
-- semantics.
lookup :: Ord key => key -> AtomicLRU key val -> IO (Maybe val)
lookup key (C mvar) = modifyMVar' mvar $ return . LRU.lookup key

-- | Remove an item from an AtomicLRU.  Returns whether the item was
-- present to be removed.
delete :: Ord key => key -> AtomicLRU key val -> IO Bool
delete key (C mvar) = modifyMVar' mvar $ return . LRU.delete key

-- | Returns the number of elements the AtomicLRU currently contains.
size :: AtomicLRU key val -> IO Int
size (C mvar) = LRU.size <$> MV.readMVar mvar

-- | A version of 'MV.modifyMVar_' that applies the modification
-- function strictly.
modifyMVar_' :: MVar a -> (a -> IO a) -> IO ()
modifyMVar_' mvar f = do
  x <- MV.takeMVar mvar
  x' <- f x
  MV.putMVar mvar $! x'

-- | A version of 'MV.modifyMVar' that applies the modification
-- function strictly. The returned value is not evaluated strictly.
modifyMVar' :: MVar a -> (a -> IO (a, b)) -> IO b
modifyMVar' mvar f = do
  x <- MV.takeMVar mvar
  (x', result) <- f x
  MV.putMVar mvar $! x'
  return result
