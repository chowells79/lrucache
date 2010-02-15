{-# LANGUAGE FlexibleInstances #-}
import Prelude hiding ( lookup )

import Control.Monad
import Control.Monad.Exception.Synchronous

import Data.Cache.LRU.Internal

import Test.QuickCheck
    ( Arbitrary(..)
    , Args(..)
    , choose
    , oneof
    , shrinkNothing
    , quickCheckWith
    , stdArgs
    )
import Test.QuickCheck.Property ( Result(..), result, succeeded )

data Action key val = Insert key val
                    | Lookup key
                      deriving (Show, Eq)

instance Arbitrary (Action Int Int) where
    arbitrary = oneof [ins, look]
        where ins = liftM2 Insert key $ choose (100, 104)
              look = liftM Lookup key
              key = choose (1, 10)

    shrink = shrinkNothing

newtype History key val = H (Int, [Action key val] -> [Action key val])

instance Arbitrary (History Int Int) where
    arbitrary = liftM2 (curry H) s h
        where s = choose (1, 5)
              h = liftM (++) arbitrary

    shrink (H (k, h)) = map (H . (,) k . (++)) . drops . h $ []
        where drops [] = []
              drops (x:xs) = xs:[x:ys | ys <- drops xs]

instance (Show key, Show val) => Show (History key val) where
    show (H (k, h)) = show (k, h [])

execute :: (Ord key, Eq val, Show key, Show val) => History key val
        -> Exceptional String (LRU key val)
execute (H (k, h)) = execute' (h []) (newLRU k)
    where
      execute' [] lru = return lru
      execute' (x:xs) lru = executeA x lru >>= execute' xs

      executeA (Insert key val) lru = execA' key val lru $ insert key val lru

      executeA (Lookup key) lru = do
        let (lru', mVal) = lookup key lru
        case mVal of
          Nothing -> do when (toList lru /= toList lru') $
                             throw "unexpected result"
                        return lru'
          Just val -> execA' key val lru lru'

      execA' key val lru lru' = do
        when (not . valid $ lru') $ throw "not valid"

        let pre = toList lru
            post = toList lru'

            naive = (key, val) : filter ((key /=) . fst) pre
            projected = if length naive <= k then naive else init naive
        when (projected /= post) $ throw "unexpected result"

        return lru'

executesProperly :: History Int Int -> Result
executesProperly h = case execute h of
                       Success _ -> succeeded
                       Exception e -> result { ok = Just False
                                             , reason = e
                                             }

main :: IO ()
main = quickCheckWith stdArgs { maxSuccess = 1000 } executesProperly
