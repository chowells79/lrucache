{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
module Data.Cache.LRU.OrderedStore where

import           Data.Cache.LRU.Class
import qualified Data.Map as Map
import           Data.Foldable (Foldable)
import           Data.Traversable (Traversable)


newtype OrderedStore key val = OS { unOS :: Map.Map key val }
    deriving (Eq, Ord, Functor, Foldable, Traversable)

instance Ord key => Store (OrderedStore key val) key val where
    sEmpty = OS Map.empty
    sNull = Map.null . unOS
    sLookup k = Map.lookup k . unOS
    sSize = fromIntegral . Map.size . unOS
    sMember k = Map.member k . unOS
    sInsert k v = OS . Map.insert k v . unOS
    sDelete k = fmap OS . Map.updateLookupWithKey (\_ _ -> Nothing) k . unOS
    sAdjust f k = OS . Map.insertWith' (\_ o -> f o) k undefined . unOS
