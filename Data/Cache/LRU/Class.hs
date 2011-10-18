{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# OPTIONS_HADDOCK not-home #-}

-- |
module Data.Cache.LRU.Class where

import Prelude hiding ( last, lookup )


-- | This class abstracts the necessary functions for working with
-- different internal representations of the linked values
class Link link key val | link -> key, link -> val where
    newLink :: val -> Maybe key -> Maybe key -> link
    value :: link -> val
    setValue :: val -> link -> link
    prev :: link -> Maybe key
    setPrev :: Maybe key -> link -> link
    next :: link -> Maybe key
    setNext :: Maybe key -> link -> link


-- | this class abstracts the necessary functions for being a store in
-- the common implementation
class Store store key val | store -> key, store -> val where
    sEmpty :: store
    sNull :: store -> Bool
    sLookup :: key -> store -> Maybe val
    sSize :: store -> Integer
    sMember :: key -> store -> Bool
    sInsert :: key -> val -> store -> store
    sDelete :: key -> store -> (store, Maybe val)
    sAdjust :: (val -> val) -> key -> store -> store
    sAdjust f k = fst . sAdjust' f k
    sAdjust' :: (val -> val) -> key -> store -> (store, Maybe val)


-- | this data type represents the result of a Capacity calculation
data CapacityResult = Good | Overflow deriving (Eq, Ord, Show, Enum)


-- | this class provides an interface to abstract out capacity operations
class Capacity cap key val | cap -> key, cap -> val where
    cAdd :: key -> val -> cap -> (cap, CapacityResult)
    cRemove :: key -> val -> cap -> (cap, CapacityResult)
    cEmpty :: cap -> cap
