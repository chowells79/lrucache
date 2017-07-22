{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Cache.LRU
import Test.Hspec
import Test.Hspec.Core.Runner
import Test.QuickCheck
import Distribution.TestSuite as TestSuite

main = hspec functionTest

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
