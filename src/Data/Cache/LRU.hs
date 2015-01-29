-- | Implements an LRU cache.
--
-- This module provides a pure LRU cache based on a doubly-linked list
-- through a Data.Map structure.  This gives O(log n) operations on
-- 'insert', 'lookup', 'delete', and 'pop', and O(n * log n) for 'toList'.
--
-- The interface this module provides is opaque.  If further control
-- is desired, the "Data.Cache.LRU.Internal" module can be used.
module Data.Cache.LRU
    ( LRU
    , newLRU
    , fromList
    , toList
    , pairs
    , keys
    , maxSize
    , insert
    , insertInforming
    , lookup
    , delete
    , pop
    , size
    )
where

import Prelude hiding ( lookup )

import Data.Cache.LRU.Internal
