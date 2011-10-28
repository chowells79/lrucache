{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
-- | This module provides access to all the internals use by the LRU
-- type.  This can be used to create data structures that violate the
-- invariants the public interface maintains.  Be careful when using
-- this module.  The 'valid' function can be used to check if an LRU
-- structure satisfies the invariants the public interface maintains.
--
-- If this degree of control isn't needed, consider using
-- "Data.Cache.LRU" instead.
module Data.Cache.LRU.Internal where

import Control.Applicative

import Data.Cache.LRU.OrderedStore
import Data.Cache.LRU.Link
import Data.Cache.LRU.Capacity
import Data.Cache.LRU.Class

import Prelude hiding (last, lookup)


-- | Stores the information that makes up an LRU cache.  This is
-- parameterized in the type of the store, the link structure, and the
-- capacity calculation types, as well as the key and value types.
data LRUImpl store link cap key val =
    LRUImpl { first    :: !(Maybe key)
            , last     :: !(Maybe key)
            , capacity :: !(cap key val)
            , content  :: !(store key (link key val))
            }


-- | An LRU that uses Data.Map as a backing store, is not strict in
-- its values, and has a fixed maximum number of entries
type LRU key val = LRUImpl OrderedStore NonStrict MaxEntries key val

-- | create a new 'LRU'
{-# INLINEABLE newLRU #-}
newLRU :: Ord key => Integer -> LRU key val
newLRU = emptyLRUImpl . maxEntries


-- | An LRU that uses Data.Map as a backing store, is not strict in
-- its values, and has no maximum number of entries
type UnlimitedLRU key val = LRUImpl OrderedStore NonStrict Unlimited key val

-- | create a new 'UnlimitedLRU'
{-# INLINEABLE newUnlimitedLRU #-}
newUnlimitedLRU :: Ord key => UnlimitedLRU key val
newUnlimitedLRU = emptyLRUImpl Unlimited


-- | An LRU that uses Data.Map as a backing store, is strict to WHNF
-- in its values, and has a fixed maximum number of entries
type StrictLRU key val = LRUImpl OrderedStore WHNFStrict MaxEntries key val

-- | create a new 'StrictLRU'
{-# INLINEABLE newStrictLRU #-}
newStrictLRU :: Ord key => Integer -> StrictLRU key val
newStrictLRU = emptyLRUImpl . maxEntries


-- | An LRU that uses Data.Map as a backing store, is strict to WHNF
-- in its values, and has no maximum number of entries
type StrictUnlimitedLRU key val =
    LRUImpl OrderedStore WHNFStrict Unlimited key val

-- | create a new 'StrictUnlimitedLRU'
{-# INLINEABLE newStrictUnlimitedLRU #-}
newStrictUnlimitedLRU :: Ord key => StrictUnlimitedLRU key val
newStrictUnlimitedLRU = emptyLRUImpl Unlimited


instance ( Eq key
         , Eq (cap key val)
         , Eq val
         , Capacity (cap key val) key val
         , Store (store key (link key val)) key (link key val)
         , Link (link key val) key val
         ) => Eq (LRUImpl store link cap key val) where
    lru1 == lru2 = toList lru1 == toList lru2 && capacity lru1 == capacity lru2


instance ( Eq key
         , Store (store key (link key val)) key (link key val)
         , Link (link key val) key val
         , Capacity (cap key val) key val
         , Show key
         , Show val
         , Show (cap key val)
         ) => Show (LRUImpl store link cap key val) where
    show l = "fromList (" ++ show (capacity l) ++ ") " ++ show (toList l)


-- | performs the 'fmap' operation on an 'LRUImpl'.  It's called out
-- as unsafe because it can violate capacity constraints, if the
-- capacity in use is sensitive to the values.  This should only be
-- used to implement 'Functor' when the capacity isn't sensitive to
-- values.
{-# INLINEABLE unsafeFmap #-}
unsafeFmap :: ( Functor (store key)
              , Functor (link key)
              , Functor (cap key)
              )
           => (a -> b)
           -> LRUImpl store link cap key a
           -> LRUImpl store link cap key b
unsafeFmap g (LRUImpl f l m c) = LRUImpl f l (fmap g m) (fmap (fmap g) c)

instance (Functor (store key), Functor (link key)) =>
    Functor (LRUImpl store link MaxEntries key) where fmap = unsafeFmap

instance (Functor (store key), Functor (link key)) =>
    Functor (LRUImpl store link Unlimited key) where fmap = unsafeFmap


-- | create a new 'LRUImpl' value
{-# INLINEABLE emptyLRUImpl #-}
emptyLRUImpl :: ( Store (store key (link key val)) key (link key val)
                , Capacity (cap key val) key val
                )
             => cap key val -- ^ The capacity of the new 'LRUImpl'
             -> LRUImpl store link cap key val
emptyLRUImpl cap = LRUImpl Nothing Nothing (cEmpty cap) sEmpty


-- | Build a new 'LRUImpl' from the given capacity and list of
-- contents, in order from most recently accessed to least recently
-- accessed.
{-# INLINEABLE fromList #-}
fromList :: ( Eq key
            , Store (store key (link key val)) key (link key val)
            , Link (link key val) key val
            , Capacity (cap key val) key val
            )
         => cap key val -- ^ The capacity of the new 'LRUImpl'
         -> [(key, val)] -- ^ The list of contents
         -> LRUImpl store link cap key val
fromList cap l = appendAll $ emptyLRUImpl cap
   where
     appendAll = foldr ins id l
     ins (k, v) = ((fst . insert k v) .)


-- | Retrieve a list view of an LRU.  The items are returned in
-- order from most recently accessed to least recently accessed.
{-# INLINEABLE toList #-}
toList :: ( Eq key
          , Store (store key (link key val)) key (link key val)
          , Link (link key val) key val
          , Capacity (cap key val) key val
          )
       => LRUImpl store link cap key val
       -> [(key, val)]
toList lru = maybe [] (listLinks . content $ lru) $ first lru
  where
    listLinks m key = case next lv of
        Nothing -> [keyval]
        Just nk -> keyval : listLinks m nk
      where
        Just lv = sLookup key m
        keyval = (key, value lv)


-- | Add an item to an LRU.  If the key was already present in the
-- LRU, the value is changed to the new value passed in.  The
-- item added is marked as the most recently accessed item in the
-- LRU returned.
--
-- If this causes the LRU to exceed its capacity, items are dropped in
-- order of least-recently used, until the capacity is good.  The list
-- in the return value consists of any @(key, val)@ pairs dropped from
-- the 'LRUImpl'
{-# INLINEABLE insert #-}
insert :: ( Eq key
          , Store (store key (link key val)) key (link key val)
          , Link (link key val) key val
          , Capacity (cap key val) key val
          )
       => key
       -> val
       -> LRUImpl store link cap key val
       -> (LRUImpl store link cap key val, [(key, val)])
insert key val lru = maybe emptyCase nonEmptyCase $ first lru
  where
    (max', res) = cAdd key val $ capacity lru

    -- this is the case for adding to an empty LRU Cache
    emptyCase = case res of
        Good -> (LRUImpl fl fl max' m', [])
        Overflow -> (lru, [(key, val)])
      where
        fl = Just key
        lv = newLink val Nothing Nothing
        m' = sInsert key lv contents

    -- this is the case for when the LRU Cache isn't empty
    contents = content lru
    present = key `sMember` contents

    nonEmptyCase firstKey = if present then hitSet else add firstKey

    -- this updates the value stored with the key, marks it as the
    -- most recently accessed, checks whether the new value
    -- overflows, and cleans it up if it overflowed
    hitSet = case r of
        Good -> (hit' key lru', [])
        Overflow -> iterDel' lru'
      where
        (m, r) = cAdd key val . fst . cRemove key oldVal $ capacity lru
        lru' = lru { content = contents', capacity = m }
        (contents', Just oldLink) = sAdjust' (setValue val) key contents
        oldVal = value oldLink

    -- create a new LRU with a new first item, and
    -- conditionally dropping items from the end
    add firstKey = case res of
        Good -> (lru', [])
        Overflow -> iterDel' lru'
      where
        -- add a new first item
        firstLV' = newLink val Nothing $ Just firstKey
        contents' = sInsert key firstLV' .
                    sAdjust (setPrev $ Just key) firstKey $
                    contents
        lru' = lru { first = Just key
                   , content = contents'
                   , capacity = max' }


{-# INLINEABLE lookup #-}
lookup :: ( Eq key
          , Store (store key (link key val)) key (link key val)
          , Link (link key val) key val
          )
       => key
       -> LRUImpl store link cap key val
       -> (LRUImpl store link cap key val, Maybe val)
lookup key lru = case sLookup key $ content lru of
    Nothing -> (lru, Nothing)
    Just lv -> (hit' key lru, Just . value $ lv)


{-# INLINEABLE delete #-}
delete :: ( Eq key
          , Store (store key (link key val)) key (link key val)
          , Link (link key val) key val
          , Capacity (cap key val) key val
          )
       => key
       -> LRUImpl store link cap key val
       -> (LRUImpl store link cap key val, Maybe val)
delete key lru = maybe (lru, Nothing) delete'' mLV
  where
    delete'' lv = (fst $ delete' key lru cont' lv, Just $ value lv)
    (cont', mLV) = sDelete key $ content lru


{-# INLINEABLE pop #-}
pop :: ( Eq key
       , Store (store key (link key val)) key (link key val)
       , Link (link key val) key val
       , Capacity (cap key val) key val
       )
    => LRUImpl store link cap key val
    -> (LRUImpl store link cap key val, Maybe (key, val))
pop lru = if size lru == 0 then (lru, Nothing) else (lru', Just pair)
  where
    Just lastKey = last lru
    (lru', Just lastVal) = delete lastKey lru
    pair = (lastKey, lastVal)


{-# INLINEABLE size #-}
size :: (Store (store key (link key val)) key (link key val))
     => LRUImpl store link cap key val
     -> Integer
size lru = sSize $ content lru


-- | Internal function.  Deletes at least one item from the end,
-- continues until the capacity is Good.  *NOT* idempotent.  Returns
-- the items deleted as its second argument, ordered from most
-- recently used to least recently used.
{-# INLINEABLE iterDel' #-}
iterDel' :: ( Eq key
            , Capacity (cap key val) key val
            , Store (store key (link key val)) key (link key val)
            , Link (link key val) key val
            )
         => LRUImpl store link cap key val
         -> (LRUImpl store link cap key val, [(key, val)])
iterDel' lru = case res' of
    Good -> (nLru, [pair])
    Overflow -> (pair:) <$> iterDel' nLru
  where
    pair = (lKey, value lLink)
    Just lKey = last lru
    (cont, Just lLink) = sDelete lKey . content $ lru
    (nLru, res') = delete' lKey lru cont lLink


-- | Internal function.  The key passed in must be present in the
-- LRU.  Moves the item associated with that key to the most
-- recently accessed position.
{-# INLINEABLE hit' #-}
hit' :: ( Eq key
        , Store (store key (link key val)) key (link key val)
        , Link (link key val) key val
        )
     => key
     -> LRUImpl store link cap key val
     -> LRUImpl store link cap key val
hit' key lru = if key == firstKey then lru else notFirst
  where
    Just firstKey = first lru
    Just lastKey = last lru
    Just lastLV = sLookup lastKey conts
    conts = content lru

    -- key wasn't already the head of the list.  Some alteration
    -- will be needed
    notFirst = if key == lastKey then replaceLast else replaceMiddle

    adjFront = sAdjust (setPrev $ Just key) firstKey .
               sAdjust (setPrev Nothing . setNext (first lru)) key

    -- key was the last entry in the list
    replaceLast = lru { first = Just key
                      , last = prev lastLV
                      , content = cLast
                      }
    Just pKey = prev lastLV
    cLast = sAdjust (setNext $ Nothing) pKey . adjFront $ conts

    -- the key wasn't the first or last key
    replaceMiddle = lru { first = Just key
                        , content = cMid
                        }
    Just keyLV = sLookup key conts
    Just prevKey = prev keyLV
    Just nextKey = next keyLV
    cMid = sAdjust (setNext $ Just nextKey) prevKey .
           sAdjust (setPrev $ Just prevKey) nextKey .
           adjFront $ conts

-- | An internal function used by 'insert' (when the cache is full)
-- and 'delete'.  This function has strict requirements on its
-- arguments in order to work properly.
--
-- As this is intended to be an internal function, the arguments were
-- chosen to avoid repeated computation, rather than for simplicity of
-- calling this function.
{-# INLINEABLE delete' #-}
delete' :: ( Eq key
           , Capacity (cap key val) key val
           , Link (link key val) key val
           , Store (store key (link key val)) key (link key val)
           )
        => key -- ^ The key must be present in the provided 'LRUImpl'
        -> LRUImpl store link cap key val -- ^ This is the 'LRUImpl' to modify
        -> store key (link key val) -- ^ this is the store from the
                                    -- previous argument, but with the
                                    -- key already removed from it.
                                    -- This isn't consistent yet, as
                                    -- it still might contain
                                    -- LinkedVals with pointers to the
                                    -- removed key.
        -> link key val  -- ^ This is the link that corresponds to the
                         -- key in the passed in LRU. It is absent
                         -- from the passed in map.
        -> (LRUImpl store link cap key val, CapacityResult)
delete' key lru cont lv = (if sNull cont then deleteOnly else deleteOne, res)
  where
    (max', res) = cRemove key (value lv) (capacity lru)

    -- delete the only item in the cache
    deleteOnly = LRUImpl Nothing Nothing max' cont

    -- delete an item that isn't the only item
    Just firstKey = first lru
    deleteOne = if firstKey == key then deleteFirst else deleteNotFirst

    -- delete the first item
    deleteFirst = lru { first = next lv
                      , content = contFirst
                      , capacity = max'
                      }
    Just nKey = next lv
    contFirst = sAdjust (setPrev Nothing) nKey cont

    -- delete an item other than the first
    Just lastKey = last lru
    deleteNotFirst = if lastKey == key then deleteLast else deleteMid

    -- delete the last item
    deleteLast = lru { last = prev lv
                     , content = contLast
                     , capacity = max'
                     }
    Just pKey = prev lv
    contLast = sAdjust (setNext Nothing) pKey cont

    -- delete an item in the middle
    deleteMid = lru { content = contMid
                    , capacity = max'
                    }
    contMid = sAdjust (setNext $ next lv ) pKey .
              sAdjust (setPrev $ prev lv ) nKey $
              cont


-- | Internal function.  This checks four structural invariants
-- of the LRU cache structure:
--
-- 1. The cache isn't overflowing.
--
-- 2. The linked list through the nodes is consistent in both directions.
--
-- 3. The linked list contains the same number of nodes as the cache.
--
-- 4. Every key in the linked list is in the 'store'.
valid :: ( Capacity (cap key val) key val
         , Store (store key (link key val)) key (link key val)
         , Link (link key val) key val
         , Eq key
         , Eq (cap key val)
         )
      => LRUImpl store link cap key val -> Bool
valid lru = capacityGood && capacityConsistent &&
            reverse orderedKeys == reverseKeys &&
            fromIntegral (size lru) == length orderedKeys &&
            all (`sMember` contents) orderedKeys
  where
    capacityGood = Good == snd recalculatedPair
    capacityConsistent = capacity lru == fst recalculatedPair
    recalculatedPair = foldl combine (emT, Good) $ toList lru
    combine (c, _) (k, v) = cAdd k v c
    emT = cEmpty $ capacity lru

    contents = content lru
    traverse _ Nothing = []
    traverse f (Just k) = let Just k' = sLookup k contents
                          in k : (traverse f . f $ k')
    orderedKeys = traverse next . first $ lru
    reverseKeys = traverse prev . last $ lru
