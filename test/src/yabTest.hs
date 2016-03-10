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

main = defaultMain tests

tests = [testGroup n ts | (n,ts) <- [
  {-("Serialize",serializeTests)-}
  ("Budget",budgetTests)
  ]]

serializeTests = [
  {-("csv_day",prop_CSVField :: Day -> Bool)-}
  testProperty "with_temp_dir" tempDirsTests
  ]

tempDirsTests = withTempDir $ [
  prop_SerializeBudget
  ]

budgetTests :: [Test]
budgetTests = [
  testProperty "add_account" prop_AddAccount
  ,testProperty "remove_account" prop_RemoveAccount
  ,testProperty "merge_account" prop_MergeAccount
  ,testProperty "new_paycheck" prop_Newpaycheck
  ]
