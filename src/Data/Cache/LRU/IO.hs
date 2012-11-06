-- | This module contains a mutable wrapping of an LRU in the IO
-- monad, providing atomic access in a concurrent environment.  All
-- calls preserve the same semantics as those in "Data.Cache.LRU", but
-- perform updates in place.  All functions use a single atomic update
-- of the backing structure.
--
-- The interface this module provides is opaque.  If further control
-- is desired, the "Data.Cache.LRU.IO.Internal" module can be used in
-- combination with "Data.Cache.LRU.Internal".
--
-- (This implementation uses an MVar for coarse locking. It's unclear
-- if anything else would give better performance, given that many
-- calls alter the head of the access list.)
module Data.Cache.LRU.IO
    ( AtomicLRU
    , newAtomicLRU
    , fromList
    , toList
    , maxSize
    , insert
    , lookup
    , delete
    , pop
    , size
    , modifyAtomicLRU
    , modifyAtomicLRU'
    )
where

import Prelude hiding ( lookup )

import Data.Cache.LRU.IO.Internal
