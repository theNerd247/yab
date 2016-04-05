{-#LANGUAGE FlexibleInstances #-}
module Main where

import Test.QuickCheck
import Test.QuickCheck.Monadic 
import Test.Budget
import Test.Serialization

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
  ]]

serializeTests :: FilePath -> [Test]
serializeTests d = [
  testProperty "csv_day" (prop_CSVField :: Day -> Bool)
  ,testProperty "budget_serialization" (prop_SerializeBudget d)
  ]

budgetTests :: [Test]
budgetTests = [
  testProperty "add_account" prop_AddAccount
  ,testProperty "remove_account" prop_RemoveAccount
  ,testProperty "merge_account" prop_MergeAccount
  ,testProperty "new_paycheck" prop_Newpaycheck
  ]
