{-#LANGUAGE FlexibleInstances #-}
module Main where

import Test.Tasty
import Test.Tasty.Runners
import Test.Tasty.Ingredients.Rerun
import Test.Tasty.QuickCheck

import Test.Data.Budget.Budget
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
  {-("Serialize",serializeTests d)-}
  ("Budget",budgetTests)
  ,("CSV",csvTests)
  ,("AccountSort",accountSortTests)
  ]]

serializeTests :: FilePath -> [TestTree]
serializeTests d = [
  testProperty "csv_day" (prop_CSVField :: Day -> Bool)
  ,testProperty "budget_serialization" (prop_SerializeBudget d)
  ]

csvTests :: [TestTree]
csvTests = [
  testProperty "parse_validDate" prop_validDate
  ,testProperty "parse_invalidDate" prop_invalidDate
  ]

budgetTests :: [TestTree]
budgetTests = [
  testProperty "add_account" prop_AddAccount
  ,testProperty "remove_account" prop_RemoveAccount
  ,testProperty "merge_account" prop_MergeAccount
  ,testProperty "new_paycheck" prop_Newpaycheck
  ]

accountSortTests :: [TestTree]
accountSortTests = [
  testProperty "csv_TransEntry" prop_csvTransEntry
  ]
