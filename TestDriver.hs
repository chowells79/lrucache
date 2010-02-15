import Prelude hiding ( lookup )

import Control.Monad
import Control.Monad.Exception.Synchronous

import Data.Cache.LRU.Internal

main :: IO ()
main = putStrLn "foo"

data Action key val = Insert key val
                    | Lookup key
                      deriving (Show, Eq)

newtype History key val = H (Int, [Action key val] -> [Action key val])

instance (Show key, Show val) => Show (History key val) where
    show (H (k, h)) = show (k, h [])

snoc :: Action key val -> History key val -> History key val
snoc a (H (k, h)) = H (k, h . (a:))

cons :: Action key val -> History key val -> History key val
cons a (H (k, h)) = H (k, (a:) . h)

empty :: Int -> History key val
empty k = H (k, id)

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

executeIO :: (Ord key, Eq val, Show key, Show val) => History key val -> IO ()
executeIO h = case execute h of
                Exception s -> putStrLn $ "Error: " ++ s
                Success c -> putStrLn $ "Success: " ++ (show . toList $ c)
