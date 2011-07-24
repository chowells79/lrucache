{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

-- |
module Data.Cache.LRU.Link where

import Data.Cache.LRU.Class


-- | These values embed a doubly-linked list through the backing store
-- of the cache.  This version is non-strict in the value it stores.
data NonStrict key val = LV val !(Maybe key) !(Maybe key) deriving (Eq, Ord)

instance Link (NonStrict key val) key val where
    newLink = LV
    value (LV v _ _) = v
    setValue v (LV _ p n) = LV v p n
    prev (LV _ p _) = p
    setPrev p (LV v _ n) = LV v p n
    next (LV _ _ n) = n
    setNext n (LV v p _) = LV v p n

instance Functor (NonStrict key) where
    fmap f (LV v p n) = LV (f v) p n


-- | These values embed a doubly-linked list through the backing store
-- of the cache.  This version is strict to WHNF in the value it stores.
data WHNFStrict key val = LV' !val !(Maybe key) !(Maybe key) deriving (Eq, Ord)

instance Link (WHNFStrict key val) key val where
    newLink = LV'
    value (LV' v _ _) = v
    setValue v (LV' _ p n) = LV' v p n
    prev (LV' _ p _) = p
    setPrev p (LV' v _ n) = LV' v p n
    next (LV' _ _ n) = n
    setNext n (LV' v p _) = LV' v p n

instance Functor (WHNFStrict key) where
    fmap f (LV' v p n) = LV' (f v) p n
