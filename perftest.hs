import Criterion.Config
import Criterion.Main

import Data.Cache.LRU.Capacity
import Data.Cache.LRU
import Data.Monoid

import System.Random

main :: IO ()
main = do
    let config = defaultConfig {
                   cfgSamples   = Last $ Just 200
                 , cfgPerformGC = Last $ Just True
                 }
        setup  = return ()
        tests  = [bench "mass insert" massInsertTest]

    defaultMainWith config setup tests


-- make this a CAF, in order to only evaluate it once
massInsertList :: [(Int, Int)]
massInsertList = take 10000 $ zip keys vals
  where
    keys = randomRs (1, 150) $ mkStdGen 80
    vals = randomRs (1000, 10000) $ mkStdGen 443


massInsertFunction :: [(Int, Int)] -> LRU Int Int
massInsertFunction = fromList $ maxEntries 100


massInsertTest :: Pure
massInsertTest = whnf massInsertFunction massInsertList

