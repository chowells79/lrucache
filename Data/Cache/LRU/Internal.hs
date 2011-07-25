{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
-- |
module Data.Cache.LRU.Internal where

import Data.Cache.LRU.OrderedStore
import Data.Cache.LRU.Link
import Data.Cache.LRU.Capacity
import Data.Cache.LRU.Class

import Prelude hiding (last, lookup, max)


-- | Stores the information that makes up an LRU cache
data LRUImpl store link cap key val =
    LRUImpl { first   :: !(Maybe key)
            , last    :: !(Maybe key)
            , max     :: !(cap key val)
            , content :: !(store key (link key val))
            }

deriving instance (Eq key, Eq (cap key val), Eq val, Eq (store key (link key val))) => Eq (LRUImpl store link cap key val)


type LRU key val = LRUImpl OrderedStore NonStrict MaxEntries key val

newLRU :: Ord key => Integer -> LRU key val
newLRU = emptyLRU . maxEntries

type UnlimitedLRU key val = LRUImpl OrderedStore NonStrict Unlimited key val

newUnlimitedLRU :: Ord key => UnlimitedLRU key val
newUnlimitedLRU = emptyLRU Unlimited

type StrictLRU key val = LRUImpl OrderedStore WHNFStrict MaxEntries key val

newStrictLRU :: Ord key => Integer -> StrictLRU key val
newStrictLRU = emptyLRU . maxEntries

type StrictUnlimitedLRU key val = LRUImpl OrderedStore WHNFStrict Unlimited key val

newStrictUnlimitedLRU :: Ord key => StrictUnlimitedLRU key val
newStrictUnlimitedLRU = emptyLRU Unlimited

instance (Eq key, Store (store key (link key val)) key (link key val), Link (link key val) key val, Capacity (cap key val) key val, Show key, Show val, Show (cap key val)) =>
    Show (LRUImpl store link cap key val) where

    show l = "fromList (" ++ show (max l) ++ ") " ++ show (toList l)

unsafeFmap :: (Functor (store key), Functor (link key), Functor (cap key)) => (a -> b) -> LRUImpl store link cap key a -> LRUImpl store link cap key b
unsafeFmap g (LRUImpl f l m c) = LRUImpl f l (fmap g m) (fmap (fmap g) c)

instance (Functor (store key), Functor (link key)) =>
    Functor (LRUImpl store link MaxEntries key) where fmap = unsafeFmap

instance (Functor (store key), Functor (link key)) =>
    Functor (LRUImpl store link Unlimited key) where fmap = unsafeFmap

instance (Eq key, Store (store key (link key val)) key (link key val), Link (link key val) key val, Capacity (cap key val) key val) =>
    LRUInterface (LRUImpl store link cap key val) (cap key val) key val where

    emptyLRU cap = LRUImpl Nothing Nothing cap sEmpty

    fromList cap l = appendAll $ emptyLRU cap
      where
        appendAll = foldr ins id l
        ins (k, v) = (insert k v .)

    toList lru = maybe [] (listLinks . content $ lru) $ first lru
      where
        listLinks m key = case next lv of
            Nothing -> [keyval]
            Just nk -> keyval : listLinks m nk
          where
            Just lv = sLookup key m
            keyval = (key, value lv)

    capacity lru = max lru

    insert key val lru = maybe emptyCase nonEmptyCase $ first lru
      where
        (res, max') = cAdd key val $ max lru

        -- this is the case for adding to an empty LRU Cache
        emptyCase = case res of
            Good -> LRUImpl fl fl max' m'
            Overflow -> lru
          where
            fl = Just key
            lv = newLink val Nothing Nothing
            m' = sInsert key lv contents

        -- this is the case for when the LRU Cache isn't empty
        contents = content lru
        present = key `sMember` contents

        nonEmptyCase firstKey = if present then hitSet else add firstKey

        -- this updates the value stored with the key, then marks it as
        -- the most recently accessed
        hitSet = hit' key lru'
          where
            lru' = lru { content = contents' }
            contents' = sAdjust (setValue val) key contents

        -- create a new LRU with a new first item, and
        -- conditionally dropping items from the end
        add firstKey = case res of
            Good -> lru' max'
            Overflow -> iterDel $ lru' max'
          where
            -- add a new first item
            firstLV' = newLink val Nothing $ Just firstKey
            contents' = sInsert key firstLV' .
                        sAdjust (setPrev $ Just key) firstKey $
                        contents
            lru' max'' = lru { first = Just key, content = contents', max = max'' }

            -- drop items from the end
            iterDel lru'' = case res' of
                Good -> nLru
                Overflow -> iterDel nLru
              where
                Just lKey = last lru''
                (Just lLink, cont) = sDelete lKey . content $ lru''
                (res', nLru) = delete' lKey lru'' cont lLink

    lookup key lru = case sLookup key $ content lru of
        Nothing -> (lru, Nothing)
        Just lv -> (hit' key lru, Just . value $ lv)

    delete key lru = maybe (lru, Nothing) delete'' mLV
      where
        delete'' lv = (snd $ delete' key lru cont' lv, Just $ value lv)
        (mLV, cont') = sDelete key $ content lru

    pop lru = if size lru == 0 then (lru, Nothing) else (lru', Just pair)
      where
        Just lastKey = last lru
        (lru', Just lastVal) = delete lastKey lru
        pair = (lastKey, lastVal)

    size lru = sSize $ content lru


-- | Internal function.  The key passed in must be present in the
-- LRU.  Moves the item associated with that key to the most
-- recently accessed position.
hit' :: (Eq key, Store (store key (link key val)) key (link key val), Link (link key val) key val) => key -> LRUImpl store link cap key val -> LRUImpl store link cap key val
hit' key lru = if key == firstKey then lru else notFirst
    where Just firstKey = first lru
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
delete' :: (Eq key, Capacity (cap key val) key val, Link (link key val) key val, Store (store key (link key val)) key (link key val))
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
        -> (CapacityResult, LRUImpl store link cap key val)
delete' key lru cont lv = (res, if sNull cont then deleteOnly else deleteOne)
  where
    (res, max') = cRemove key (value lv) (max lru)

    -- delete the only item in the cache
    deleteOnly = LRUImpl Nothing Nothing max' cont

    -- delete an item that isn't the only item
    Just firstKey = first lru
    deleteOne = if firstKey == key then deleteFirst else deleteNotFirst

    -- delete the first item
    deleteFirst = lru { first = next lv
                      , content = contFirst
                      , max = max'
                      }
    Just nKey = next lv
    contFirst = sAdjust (setPrev Nothing) nKey cont

    -- delete an item other than the first
    Just lastKey = last lru
    deleteNotFirst = if lastKey == key then deleteLast else deleteMid

    -- delete the last item
    deleteLast = lru { last = prev lv
                     , content = contLast
                     , max = max'
                     }
    Just pKey = prev lv
    contLast = sAdjust (setNext Nothing) pKey cont

    -- delete an item in the middle
    deleteMid = lru { content = contMid
                    , max = max'
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
valid lru = capacityValid &&
            reverse orderedKeys == reverseKeys &&
            fromIntegral (size lru) == length orderedKeys &&
            all (`sMember` contents) orderedKeys
  where
    capacityValid      = capacityGood && capacityConsistent
    capacityGood       = Good == fst recalculatedPair
    capacityConsistent = max lru == snd recalculatedPair
    recalculatedPair = foldl (\(_,c) (k,v) -> cAdd k v c) (Good, emT) $ toList lru
    emT = cEmpty $ max lru

    contents = content lru
    orderedKeys = traverse next . first $ lru
    traverse _ Nothing = []
    traverse f (Just k) = let Just k' = sLookup k contents
                          in k : (traverse f . f $ k')
    reverseKeys = traverse prev . last $ lru
