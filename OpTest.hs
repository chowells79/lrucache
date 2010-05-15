{-# LANGUAGE FlexibleInstances #-}
import qualified Prelude
import Prelude hiding ( lookup, last )

import Control.Applicative
import Control.Monad
import Control.Monad.Exception.Synchronous

import Data.Cache.LRU.Internal

import Test.QuickCheck
    ( Arbitrary(..)
    , Args(..)
    , Gen
    , choose
    , oneof
    , shrinkNothing
    , quickCheckWith
    , stdArgs
    )
import Test.QuickCheck.Property ( Result(..), result, succeeded )

data Action key val = Insert key val
                    | Lookup key
                    | Delete key
                    | Pop
                      deriving (Show, Eq)

instance Arbitrary (Action Int Int) where
    arbitrary = oneof [ins, look, del, pop]
        where ins = liftM2 Insert key $ choose (100, 104)
              look = liftM Lookup key
              del = liftM Delete key
              pop = return Pop
              key = choose (1, 10)

    shrink = shrinkNothing

newtype History key val = H (Maybe Int, [Action key val] -> [Action key val])

instance Arbitrary (History Int Int) where
    arbitrary = liftM2 (curry H) s h
        where s = liftM2 (<$) (choose (1, 5)) (arbitrary :: Gen (Maybe ()))
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

      execA' key val lru lru' = do
        when (not . valid $ lru') $ throw "not valid"

        let pre = toList lru
            post = toList lru'

            naive = (key, val) : filter ((key /=) . fst) pre
            sizeOk = maybe True (length naive <=) k
            projected = if sizeOk then naive else init naive
        when (projected /= post) $ throw "unexpected result"

        return lru'

      executeA (Delete key) lru = do
        let (lru', removed) = delete key lru
        when (not . valid $ lru') $ throw "not valid"

        let pre = toList lru
            post = toList lru'
            projected = filter ((key /=) . fst) pre
            expectedRemoval = Prelude.lookup key pre

        when (removed /= expectedRemoval) $ throw "unexpected value removed"
        when (projected /= post) $ throw "unexpected resulting lru"
        return lru'

      executeA (Insert key val) lru = execA' key val lru $ insert key val lru

      executeA (Lookup key) lru = case mVal of
                                    Nothing -> checkSame
                                    Just val -> execA' key val lru lru'
          where (lru', mVal) = lookup key lru
                checkSame = do when (toList lru /= toList lru') $
                                    throw "unexpected result"
                               return lru'

      executeA Pop lru = do
        let (lru', popped) = pop lru
        when (not . valid $ lru') $ throw "not valid"

        let pre = toList lru
            post = toList lru'

            (ePost, ePopped) = case pre of
                                 [] -> ([], Nothing)
                                 _  -> (init pre, Just $ Prelude.last pre)

        when (post /= ePost) $ throw "unexpected result lru"
        when (popped /= ePopped) $ throw "unexpected result key-value"
        return lru'


executesProperly :: History Int Int -> Result
executesProperly h = case execute h of
                       Success _ -> succeeded
                       Exception e -> result { ok = Just False
                                             , reason = e
                                             }

main :: IO ()
main = quickCheckWith stdArgs { maxSuccess = 1000 } executesProperly
