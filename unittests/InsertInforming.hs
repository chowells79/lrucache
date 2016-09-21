{-# LANGUAGE ScopedTypeVariables #-}

module InsertInforming where

import Data.Cache.LRU
import Test.Hspec
import Test.Hspec.Core.Runner
import Test.QuickCheck
import Distribution.TestSuite as TestSuite


tests :: IO [Test]
tests = return . map (uncurry hspecToTest) $
        [ (functionTest,"cache functional tests") ]

hspecToTest :: Spec -> String -> Test
hspecToTest s = Test . hspecToTestInstance s

hspecToTestInstance :: Spec -> String -> TestInstance
hspecToTestInstance tests name =
  progressToTestInstance name .
  fmap (Finished . summaryToResult) $
  hspecResult tests
  where
    summaryToResult summary
      | fails == 0 = Pass
      | otherwise = Fail $ show fails ++ " of " ++ show runs ++ " FAILED."
      where fails = summaryFailures summary
            runs = summaryExamples summary
    progressToTestInstance n progressAction =
      TestInstance { TestSuite.run = progressAction
                   , name = n
                   , tags = []
                   , options = []
                   , setOption = (const . const)
                                 (Right $ hspecToTestInstance tests name)
                   }

functionTest :: Spec
functionTest =
  describe "Data.Cache.LRU" $ do
  it "drops the least recently added item" $ do
    property $ \ (Positive (cacheSize :: Int)) ->
      snd $
      foldl
      (\ (lru,ok) n ->
        if not ok
        then (lru, False)
        else case insertInforming n n lru of
               (lru', Nothing) -> (lru', True)
               (lru', Just (m,_)) ->
                 if m == n - cacheSize
                 then (lru', True)
                 else (lru', False)
      )
      (newLRU (Just (fromIntegral cacheSize)), True)
      [1..cacheSize * 2]
    
