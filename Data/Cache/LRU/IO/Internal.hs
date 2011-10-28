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

import Prelude hiding ( lookup, mod, take )

import Control.Applicative ( (<$>) )
import Control.Concurrent.MVar ( MVar )
import qualified Control.Concurrent.MVar as MV

import Control.Exception ( bracketOnError )

import Data.Cache.LRU.Internal ( LRUImpl )
import qualified Data.Cache.LRU.Internal as LRU

import Data.Cache.LRU.OrderedStore
import Data.Cache.LRU.Link
import Data.Cache.LRU.Capacity
import Data.Cache.LRU.Class (Store, Link, Capacity)


-- | The opaque wrapper type
newtype AtomicLRUImpl store link cap key val =
    C (MVar (LRUImpl store link cap key val))


type AtomicLRU key val = AtomicLRUImpl OrderedStore NonStrict MaxEntries key val

newAtomicLRU :: Ord key => Integer -> IO (AtomicLRU key val)
newAtomicLRU = emptyAtomicLRUImpl . maxEntries


type AtomicUnlimitedLRU key val =
    AtomicLRUImpl OrderedStore NonStrict Unlimited key val

newAtomicUnlimitedLRU :: Ord key => IO (AtomicUnlimitedLRU key val)
newAtomicUnlimitedLRU = emptyAtomicLRUImpl Unlimited


type AtomicStrictLRU key val =
    AtomicLRUImpl OrderedStore WHNFStrict MaxEntries key val

newAtomicStrictLRU :: Ord key => Integer -> IO (AtomicStrictLRU key val)
newAtomicStrictLRU = emptyAtomicLRUImpl . maxEntries


type AtomicStrictUnlimitedLRU key val =
    AtomicLRUImpl OrderedStore WHNFStrict Unlimited key val

newAtomicStrictUnlimitedLRU :: Ord key => IO (AtomicStrictUnlimitedLRU key val)
newAtomicStrictUnlimitedLRU = emptyAtomicLRUImpl Unlimited



-- | Make a new AtomicLRU that will not grow beyond the optional
-- maximum size, if specified.
emptyAtomicLRUImpl :: ( Store (store key (link key val)) key (link key val)
                      , Capacity (cap key val) key val
                      )
                   => cap key val
                   -> IO (AtomicLRUImpl store link cap key val)
emptyAtomicLRUImpl = fmap C . MV.newMVar . LRU.emptyLRUImpl


-- | Build a new LRU from the optional maximum size and list of
-- contents. See 'LRU.fromList' for the semantics.
fromList :: ( Eq key
            , Store (store key (link key val)) key (link key val)
            , Link (link key val) key val
            , Capacity (cap key val) key val
            )
         => cap key val
         -> [(key, val)]
         -> IO (AtomicLRUImpl store link cap key val)
fromList s l = fmap C . MV.newMVar $ LRU.fromList s l


-- | Retrieve a list view of an AtomicLRU.  See 'LRU.toList' for the
-- semantics.
toList :: ( Eq key
          , Store (store key (link key val)) key (link key val)
          , Link (link key val) key val
          , Capacity (cap key val) key val
          )
       => AtomicLRUImpl store link cap key val
       -> IO [(key, val)]
toList (C mvar) = LRU.toList <$> MV.readMVar mvar


-- |
capacity :: AtomicLRUImpl store link cap key val -> IO (cap key val)
capacity (C mvar) = LRU.capacity <$> MV.readMVar mvar


-- | Insert a key/value pair into an AtomicLRU.  See 'LRU.insert' for
-- the semantics.
insert :: ( Eq key
          , Store (store key (link key val)) key (link key val)
          , Link (link key val) key val
          , Capacity (cap key val) key val
          )
       => key
       -> val
       -> AtomicLRUImpl store link cap key val
       -> IO [(key, val)]
insert key val (C mvar) = modifyMVar' mvar $ return . LRU.insert key val


-- | Look up a key in an AtomicLRU. See 'LRU.lookup' for the
-- semantics.
lookup :: ( Eq key
          , Store (store key (link key val)) key (link key val)
          , Link (link key val) key val
          )
       => key
       -> AtomicLRUImpl store link cap key val
       -> IO (Maybe val)
lookup key (C mvar) = modifyMVar' mvar $ return . LRU.lookup key


-- | Remove an item from an AtomicLRU.  Returns the value for the
-- removed key, if it was present
delete :: ( Eq key
          , Store (store key (link key val)) key (link key val)
          , Link (link key val) key val
          , Capacity (cap key val) key val
          )
       => key
       -> AtomicLRUImpl store link cap key val
       -> IO (Maybe val)
delete key (C mvar) = modifyMVar' mvar $ return . LRU.delete key


-- | Remove the least-recently accessed item from an AtomicLRU.
-- Returns the (key, val) pair removed, if one was present.
pop :: ( Eq key
       , Store (store key (link key val)) key (link key val)
       , Link (link key val) key val
       , Capacity (cap key val) key val
       )
    => AtomicLRUImpl store link cap key val
    -> IO (Maybe (key, val))
pop (C mvar) = modifyMVar' mvar $ return . LRU.pop


-- | Returns the number of elements the AtomicLRU currently contains.
size :: (Store (store key (link key val)) key (link key val))
     => AtomicLRUImpl store link cap key val
     -> IO Integer
size (C mvar) = LRU.size <$> MV.readMVar mvar


-- | Given a function that takes an 'LRU.LRU' and returns one of the
-- same type, use it to modify the contents of this AtomicLRU.
modifyAtomicLRU :: (LRUImpl store link cap key val ->
                        LRUImpl store link cap key val)
                -> AtomicLRUImpl store link cap key val
                -> IO ()
modifyAtomicLRU f = modifyAtomicLRU' $ return . f


-- | Given a function that takes an 'LRU.LRU' and returns an IO action
-- producting one of the same type, use it to modify the contents of
-- this AtomicLRU.
modifyAtomicLRU' :: (LRUImpl store link cap key val ->
                         IO (LRUImpl store link cap key val))
                 -> AtomicLRUImpl store link cap key val
                 -> IO ()
modifyAtomicLRU' f (C mvar) = modifyMVar_' mvar f


-- | A version of 'MV.modifyMVar_' that forces the result of the
-- function application to WHNF.
modifyMVar_' :: MVar a -> (a -> IO a) -> IO ()
modifyMVar_' mvar f = do
  let take = MV.takeMVar mvar
      replace = MV.putMVar mvar
      mod x = do
        x' <- f x
        MV.putMVar mvar $! x'

  bracketOnError take replace mod


-- | A version of 'MV.modifyMVar' that forces the result of the
-- function application to WHNF.
modifyMVar' :: MVar a -> (a -> IO (a, b)) -> IO b
modifyMVar' mvar f = do
  let take = MV.takeMVar mvar
      replace = MV.putMVar mvar
      mod x = do
        (x', result) <- f x
        MV.putMVar mvar $! x'
        return result

  bracketOnError take replace mod
