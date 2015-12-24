{-#LANGUAGE FlexibleInstances #-}
{-|
Module      : Name
Description : Test suite for the Data.Budget module
Copyright   : 
License     : GPL-2
Maintainer  : theNerd247
Stability   : experimental
Portability : POSIX


-}

module Test.Budget
(
)
where

import Test.QuickCheck
import Data.Budget
import Test.LoremWords

import qualified Data.Map as DM

instance Arbitrary BudgetAccount where
  arbitrary = do 
    fmap DM.fromList $ listOf1 genItem
    where
      genItem = do
        n <- loremWord
        (Positive a) <- arbitrary
        return (n,a)

instance Arbitrary Budget where
  arbitrary = do
    (Positive i) <- arbitrary
    (Positive r) <- arbitrary
    ba <- arbitrary
    return $ Budget 
      {
       budgetIncome = i
      ,budgetRate = r
      ,budgetAccounts = ba 
      }
