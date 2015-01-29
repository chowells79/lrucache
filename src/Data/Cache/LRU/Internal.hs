{-# OPTIONS_HADDOCK not-home #-}
{-# LANGUAGE CPP, DeriveDataTypeable, DeriveFunctor,
             DeriveFoldable, DeriveTraversable #-}

-- | This module provides access to all the internals use by the LRU
-- type.  This can be used to create data structures that violate the
-- invariants the public interface maintains.  Be careful when using
-- this module.  The 'valid' function can be used to check if an LRU
-- structure satisfies the invariants the public interface maintains.
--
-- If this degree of control isn't needed, consider using
-- "Data.Cache.LRU" instead.
module Data.Cache.LRU.Internal where

import Control.Applicative (Applicative, pure, liftA2)
import Data.Traversable (Traversable(traverse), foldMapDefault)
import Data.Foldable (Foldable(foldMap), traverse_)

import Prelude hiding (last, lookup)

import Data.Map ( Map )
import qualified Data.Map as Map
#if MIN_VERSION_containers(0,5,0)
import qualified Data.Map.Strict as MapStrict
#endif

import Data.Data (Data)
import Data.Typeable (Typeable)

import Data.Functor.Contravariant (Contravariant((>$)))

-- | Stores the information that makes up an LRU cache
data LRU key val = LRU {
      first :: !(Maybe key) -- ^ the key of the most recently accessed entry
    , last :: !(Maybe key) -- ^ the key of the least recently accessed entry
    , maxSize :: !(Maybe Integer) -- ^ the maximum size of the LRU cache
    , content :: !(Map key (LinkedVal key val)) -- ^ the backing 'Map'
    } deriving (Eq, Data, Typeable, Functor)

instance (Ord key) => Traversable (LRU key) where
    traverse f l = fmap (fromList $ maxSize l) . go $ toList l
      where
        go [] = pure []
        go (x:xs) = liftA2 (:) (g x) (go xs)
        g (a, b) = fmap ((,) a) $ f b

instance (Ord key) => Foldable (LRU key) where
    foldMap = foldMapDefault

instance (Ord key, Show key, Show val) => Show (LRU key val) where
    show lru = "fromList " ++ show (toList lru)

-- | The values stored in the Map of the LRU cache.  They embed a
-- doubly-linked list through the values of the 'Map'.
data LinkedVal key val = Link {
      value :: val -- ^ The actual value
    , prev :: !(Maybe key) -- ^ the key of the value before this one
    , next :: !(Maybe key) -- ^ the key of the value after this one
    } deriving (Eq, Data, Typeable, Functor, Foldable, Traversable)

-- | Make an LRU.  If a size limit is specified, the LRU is guaranteed
-- to not grow above the specified number of entries.
newLRU :: (Ord key) => Maybe Integer -- ^ the optional maximum size of the LRU
       -> LRU key val
newLRU (Just s) | s <= 0 = error "non-positive size LRU"
newLRU s  = LRU Nothing Nothing s Map.empty

-- | Build a new LRU from the given maximum size and list of contents,
-- in order from most recently accessed to least recently accessed.
fromList :: Ord key => Maybe Integer -- ^ the optional maximum size of the LRU
         -> [(key, val)] -> LRU key val
fromList s l = appendAll $ newLRU s
    where appendAll = foldr ins id l
          ins (k, v) = (insert k v .)

-- | Retrieve a list view of an LRU.  The items are returned in
-- order from most recently accessed to least recently accessed.
toList :: Ord key => LRU key val -> [(key, val)]
toList lru = maybe [] (listLinks . content $ lru) $ first lru
    where
      listLinks m key =
          let Just lv = Map.lookup key m
              keyval = (key, value lv)
          in case next lv of
               Nothing -> [keyval]
               Just nk -> keyval : listLinks m nk

-- | Traverse the (key, value) pairs of the LRU, in a read-only
-- way. This is a 'Fold' in the sense used by the
-- <https://hackage.haskell.org/package/lens lens package>. It must be
-- read-only because alterations could break the underlying 'Map'
-- structure.
pairs :: (Ord key, Applicative f, Contravariant f)
      => ((key, val) -> f (key, val))
      -> LRU key val -> f (LRU key val)
pairs f l = () >$ (traverse_ f $ toList l)

-- | Traverse the keys of the LRU, in a read-only
-- way. This is a 'Fold' in the sense used by the
-- <https://hackage.haskell.org/package/lens lens package>. It must be
-- read-only because alterations could break the underlying 'Map'
-- structure.
keys :: (Ord key, Applicative f, Contravariant f)
     => (key -> f key)
     -> LRU key val -> f (LRU key val)
keys f l = () >$ (traverse_ (f . fst) $ toList l)

-- | Add an item to an LRU.  If the key was already present in the
-- LRU, the value is changed to the new value passed in.  The
-- item added is marked as the most recently accessed item in the
-- LRU returned.
--
-- If this would cause the LRU to exceed its maximum size, the
-- least recently used item is dropped from the cache.
insert :: Ord key => key -> val -> LRU key val -> LRU key val
insert key val lru = fst (insertInforming key val lru)

-- | Same as 'insert', but also returns element which was dropped from
-- cache, if any.
insertInforming :: Ord key => key -> val -> LRU key val
                -> (LRU key val, Maybe (key, val))
insertInforming key val lru = maybe emptyCase nonEmptyCase $ first lru
    where
      contents = content lru
      full = maybe False (fromIntegral (Map.size contents) ==) $ maxSize lru
      present = key `Map.member` contents

      -- this is the case for adding to an empty LRU Cache
      emptyCase = (LRU fl fl (maxSize lru) m', Nothing)
          where
            fl = Just key
            lv = Link val Nothing Nothing
            m' = Map.insert key lv contents

      nonEmptyCase firstKey = if present then (hitSet, Nothing)
                              else add firstKey

      -- this updates the value stored with the key, then marks it as
      -- the most recently accessed
      hitSet = hit' key lru'
          where lru' = lru { content = contents' }
                contents' = adjust' (\v -> v {value = val}) key contents

      -- create a new LRU with a new first item, and
      -- conditionally dropping the last item
      add firstKey = if full then (lru'', Just (key, val))
                     else (lru', Nothing)
          where
            -- add a new first item
            firstLV' = Link val Nothing $ Just firstKey
            contents' = Map.insert key firstLV' .
                        adjust' (\v -> v { prev = Just key }) firstKey $
                        contents
            lru' = lru { first = Just key, content = contents' }

            -- remove the last item
            Just lastKey = last lru'
            Just lastLV = Map.lookup lastKey contents'
            contents'' = Map.delete lastKey contents'
            lru'' = delete' lastKey lru' contents'' lastLV

-- | Look up an item in an LRU.  If it was present, it is marked as
-- the most recently accesed in the returned LRU.
lookup :: Ord key => key -> LRU key val -> (LRU key val, Maybe val)
lookup key lru = case Map.lookup key $ content lru of
                           Nothing -> (lru, Nothing)
                           Just lv -> (hit' key lru, Just . value $ lv)

-- | Remove an item from an LRU.  Returns the new LRU, and the value
-- removed if the key was present.
delete :: Ord key => key -> LRU key val -> (LRU key val, Maybe val)
delete key lru = maybe (lru, Nothing) delete'' mLV
    where
      delete'' lv = (delete' key lru cont' lv, Just $ value lv)
      (mLV, cont') = Map.updateLookupWithKey (\_ _ -> Nothing) key $ content lru

-- | Removes the least-recently accessed element from the LRU.
-- Returns the new LRU, and the key and value from the least-recently
-- used element, if there was one.
pop :: Ord key => LRU key val -> (LRU key val, Maybe (key, val))
pop lru = if size lru == 0 then (lru, Nothing) else (lru', Just pair)
    where
      Just lastKey = last lru
      (lru', Just lastVal) = delete lastKey lru
      pair = (lastKey, lastVal)

-- | Returns the number of elements the LRU currently contains.
size :: LRU key val -> Int
size = Map.size . content

-- | Internal function.  The key passed in must be present in the
-- LRU.  Moves the item associated with that key to the most
-- recently accessed position.
hit' :: Ord key => key -> LRU key val -> LRU key val
hit' key lru = if key == firstKey then lru else notFirst
    where Just firstKey = first lru
          Just lastKey = last lru
          Just lastLV = Map.lookup lastKey conts
          conts = content lru

          -- key wasn't already the head of the list.  Some alteration
          -- will be needed
          notFirst = if key == lastKey then replaceLast else replaceMiddle

          adjFront = adjust' (\v -> v { prev = Just key}) firstKey .
                     adjust' (\v -> v { prev = Nothing
                                      , next = first lru }) key

          -- key was the last entry in the list
          replaceLast = lru { first = Just key
                            , last = prev lastLV
                            , content = cLast
                            }
          Just pKey = prev lastLV
          cLast = adjust' (\v -> v { next = Nothing }) pKey . adjFront $ conts

          -- the key wasn't the first or last key
          replaceMiddle = lru { first = Just key
                              , content = cMid
                              }
          Just keyLV = Map.lookup key conts
          Just prevKey = prev keyLV
          Just nextKey = next keyLV
          cMid = adjust' (\v -> v { next = Just nextKey }) prevKey .
                 adjust' (\v -> v { prev = Just prevKey }) nextKey .
                 adjFront $ conts

-- | An internal function used by 'insert' (when the cache is full)
-- and 'delete'.  This function has strict requirements on its
-- arguments in order to work properly.
--
-- As this is intended to be an internal function, the arguments were
-- chosen to avoid repeated computation, rather than for simplicity of
-- calling this function.
delete' :: Ord key => key -- ^ The key must be present in the provided 'LRU'
        -> LRU key val -- ^ This is the 'LRU' to modify
        -> Map key (LinkedVal key val) -- ^ this is the 'Map' from the
                                       -- previous argument, but with
                                       -- the key already removed from
                                       -- it.  This isn't consistent
                                       -- yet, as it still might
                                       -- contain LinkedVals with
                                       -- pointers to the removed key.
        -> LinkedVal key val -- ^ This is the 'LinkedVal' that
                             -- corresponds to the key in the passed
                             -- in LRU. It is absent from the passed
                             -- in map.
        -> LRU key val
delete' key lru cont' lv = if Map.null cont' then deleteOnly else deleteOne
    where
      -- delete the only item in the cache
      deleteOnly = lru { first = Nothing
                       , last = Nothing
                       , content = cont'
                       }

      -- delete an item that isn't the only item
      Just firstKey = first lru
      deleteOne = if firstKey == key then deleteFirst else deleteNotFirst

      -- delete the first item
      deleteFirst = lru { first = next lv
                        , content = contFirst
                        }
      Just nKey = next lv
      contFirst = adjust' (\v -> v { prev = Nothing }) nKey cont'

      -- delete an item other than the first
      Just lastKey = last lru
      deleteNotFirst = if lastKey == key then deleteLast else deleteMid

      -- delete the last item
      deleteLast = lru { last = prev lv
                       , content = contLast
                       }
      Just pKey = prev lv
      contLast = adjust' (\v -> v { next = Nothing}) pKey cont'

      -- delete an item in the middle
      deleteMid = lru { content = contMid }
      contMid = adjust' (\v -> v { next = next lv }) pKey .
                adjust' (\v -> v { prev = prev lv }) nKey $
                cont'

-- | Internal function.  This is very similar to 'Map.adjust', with
-- two major differences.  First, it's strict in the application of
-- the function, which is a huge win when working with this structure.
--
-- Second, it requires that the key be present in order to work.  If
-- the key isn't present, 'undefined' will be inserted into the 'Map',
-- which will cause problems later.
adjust' :: Ord k => (a -> a) -> k -> Map k a -> Map k a
#if MIN_VERSION_containers(0,5,0)
adjust' = MapStrict.adjust
#else
adjust' f k m = Map.insertWith' (\_ o -> f o) k (error "adjust' used wrongly") m
#endif

-- | Internal function.  This checks the four structural invariants
-- of the LRU cache structure:
--
-- 1. The cache's size does not exceed the specified max size.
--
-- 2. The linked list through the nodes is consistent in both directions.
--
-- 3. The linked list contains the same number of nodes as the cache.
--
-- 4. Every key in the linked list is in the 'Map'.
valid :: Ord key => LRU key val -> Bool
valid lru = maybe True (fromIntegral (size lru) <=) (maxSize lru) &&
            reverse orderedKeys == reverseKeys &&
            size lru == length orderedKeys &&
            all (`Map.member` contents) orderedKeys
    where contents = content lru
          orderedKeys = walk next . first $ lru
          walk _ Nothing = []
          walk f (Just k) = let Just k' = Map.lookup k contents
                                in k : (walk f . f $ k')
          reverseKeys = walk prev . last $ lru
