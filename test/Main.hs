{-#LANGUAGE FlexibleInstances #-}
module Main where

import Test.Tasty
import Test.Tasty.Runners
import Test.Tasty.Ingredients.Rerun
import Test.Tasty.QuickCheck

import Test.Data.Budget
import Test.Data.Serialization
import Test.Data.Serialization.Csv
import Test.Yab.AccountSort

import Data.Budget
import Data.Serialization

import YabCommon

main = defaultMainWithIngredients 
  [
    rerunningTests defaultIngredients
  ] tests

tests :: TestTree 
tests = testGroup "Tests" [testGroup n ts | (n,ts) <- [
  ("Serialize",serializeTests)
  ,("Budget",budgetTests)
  ,("CSV",csvTests)
  ,("AccountSort",accountSortTests)
  ]]
