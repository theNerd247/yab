{-#LANGUAGE FlexibleInstances #-}
module Main where

import Test.QuickCheck
import Test.QuickCheck.Monadic 

import Test.Data.Budget.Budget
import Test.Data.Serialization
import Test.Data.Serialization.Csv
import Test.Yab.AccountSort

import Data.Budget
import Data.Serialization

import Test.Framework(defaultMain,testGroup,Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import YabCommon

main = do
  d <- mktempDirs
  putStrLn $ "Saving Test Data To: " ++ d
  defaultMain $ tests d 

tests :: FilePath -> [Test]
tests d = [testGroup n ts | (n,ts) <- [
  ("Serialize",serializeTests d)
  ,("Budget",budgetTests)
  ,("CSV",csvTests)
  ,("AccountSort",accountSortTests)
  ]]

serializeTests :: FilePath -> [Test]
serializeTests d = [
  testProperty "csv_day" (prop_CSVField :: Day -> Bool)
  ,testProperty "budget_serialization" (prop_SerializeBudget d)
  ]

csvTests :: [Test]
csvTests = [
  testProperty "parse_validDate" prop_validDate
  ,testProperty "parse_invalidDate" prop_invalidDate
  ]

budgetTests :: [Test]
budgetTests = [
  testProperty "add_account" prop_AddAccount
  ,testProperty "remove_account" prop_RemoveAccount
  ,testProperty "merge_account" prop_MergeAccount
  ,testProperty "new_paycheck" prop_Newpaycheck
  ]

accountSortTests :: [Test]
accountSortTests = [
  testProperty "csv_TransEntry" prop_csvTransEntry
  ]
