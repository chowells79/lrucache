{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

-- |
module Data.Cache.LRU.Capacity where

import Data.Cache.LRU.Class


data MaxEntries key val = MaxEntries Integer Integer deriving (Show, Ord, Eq)

maxEntries :: Integer -> MaxEntries key val
maxEntries = MaxEntries 0

instance Capacity (MaxEntries key val) key val where
    cAdd _ _ (MaxEntries c m) = ( if ic <= m then Good else Overflow
                                , MaxEntries ic m
                                )
      where
        ic = c + 1

    cRemove _ _ (MaxEntries c m) =  ( if dc <= m then Good else Overflow
                                    , MaxEntries dc m
                                    )
      where
        dc = c - 1

instance Functor (MaxEntries key) where
    fmap _ (MaxEntries c m) = MaxEntries c m


data Unlimited key val = Unlimited deriving (Show, Ord, Eq)

instance Capacity (Unlimited key val) key val where
    cAdd _ _ _ = (Good, Unlimited)
    cRemove _ _ _ = (Good, Unlimited)

instance Functor (Unlimited key) where
    fmap _ Unlimited = Unlimited
