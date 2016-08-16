{-# LANGUAGE FlexibleInstances #-}

module Main where

import           Test.Tasty
import           Test.Tasty.Runners
import           Test.Tasty.Ingredients.Rerun
import           Test.Tasty.QuickCheck

import           Test.Data.Budget
import           Test.Yab.MongoDB

main = defaultMainWithIngredients [ rerunningTests defaultIngredients ] tests

tests :: TestTree
tests = testGroup "Tests"
                  [ testGroup n ts
                  | (n, ts) <- [ ("Budget", budgetTests)
                               , ("MongoDB", mongoDBTests)
                               ] ]
