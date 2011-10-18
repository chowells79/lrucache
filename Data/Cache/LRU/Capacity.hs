{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

-- |
module Data.Cache.LRU.Capacity where

import Data.Cache.LRU.Class


data MaxEntries key val = MaxEntries Integer Integer deriving (Show, Ord, Eq)

maxEntries :: Integer -> MaxEntries key val
maxEntries = MaxEntries 0

instance Capacity (MaxEntries key val) key val where
    cAdd _ _ (MaxEntries c m) = ( MaxEntries ic m
                                , if ic <= m then Good else Overflow
                                )
      where
        ic = c + 1

    cRemove _ _ (MaxEntries c m) =  ( MaxEntries dc m
                                    , if dc <= m then Good else Overflow
                                    )
      where
        dc = c - 1

    cEmpty (MaxEntries _ m) = MaxEntries 0 m

instance Functor (MaxEntries key) where
    fmap _ (MaxEntries c m) = MaxEntries c m


data Unlimited key val = Unlimited deriving (Show, Ord, Eq)

instance Capacity (Unlimited key val) key val where
    cAdd _ _ _ = (Unlimited, Good)
    cRemove _ _ _ = (Unlimited, Good)
    cEmpty _ = Unlimited

instance Functor (Unlimited key) where
    fmap _ Unlimited = Unlimited
