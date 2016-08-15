{-# LANGUAGE FlexibleInstances #-}

{-|
Module      : Name
Description : Test suite for the Data.Budget module
Copyright   : 
License     : GPL-2
Maintainer  : theNerd247
Stability   : experimental
Portability : POSIX


-}
module Test.Data.Budget
    ( budgetTests
    , addBudgetValidInd_prop
    , mkValidInds
    , mkTree
    ) where

import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Test.QuickCheck
import           Test.QuickCheck.Monadic
import           Data.Budget
import           Test.LoremWords
import           Data.Tree
import           Data.Tree.Lens
import           Control.Lens
import qualified Data.List               as DL

newtype BalBudget = BalBudget Budget
    deriving (Show)

newtype UnbalBudget = UnbalBudget Budget
    deriving (Show)

newtype ValidInd = ValidInd (Budget, BudgetIndex)
    deriving (Show)

instance Arbitrary BudgetData where
    arbitrary = do
        amnt <- posNum
        name <- loremWord
        return $ BudgetData { _budgetName = name, _budgetAmount = amnt }

instance (Arbitrary a) =>
         Arbitrary (Tree a) where
    arbitrary = sized mkTree

instance Arbitrary BalBudget where
    arbitrary = do
        b <- arbitrary
        return . BalBudget $ b & budgetIncome .~ (budgetExpenses b)

instance Arbitrary UnbalBudget where
    arbitrary = do
        rt <- arbitrary
        bs <- arbitrary >>= return . getNonEmpty
        return . UnbalBudget $ Node rt bs

instance Arbitrary ValidInd where
    arbitrary = do
        b <- arbitrary
        inds <- mkValidInds b
        return (ValidInd (b, inds))

mkTree :: (Arbitrary a) => Int -> Gen (Tree a)
mkTree 0 = Node <$> arbitrary <*> (pure [])
mkTree n
    | n > 15 = mkTree 15
    | n > 0 = Node <$> arbitrary
                   <*> (do
                            x <- choose (0, n)
                            vectorOf x $ mkTree (n `div` 2))

mkValidInds :: Budget -> Gen BudgetIndex
mkValidInds b = choose (-1, (length $ b ^. branches) - 1) >>= newInds
  where
    newInds i
        | i == -1 = return []
        | otherwise = maybe (return [])
                            (\nb -> mkValidInds nb >>= return . (i :))
                            (b ^? branches . ix i)

posNum = arbitrary >>= return . getPositive

checkBudgetBalanced_prop :: BalBudget -> Bool
checkBudgetBalanced_prop (BalBudget b) =
    checkBudgetBalanced b

checkBudgetBalancedInv_prop :: UnbalBudget -> Bool
checkBudgetBalancedInv_prop (UnbalBudget b) =
    not $ checkBudgetBalanced b

addBudgetValidInd_prop :: ValidInd -> Budget -> Bool
addBudgetValidInd_prop (ValidInd (old, ind)) new =
    maybe False (== new) (tst ^? treeNode ind . branches . ix 0)
  where
    tst = addBudget new ind old

budgetTests = [ (testProperty "checkBudgetBalanced" checkBudgetBalanced_prop)
              , (testProperty "checkBudgetBalancedInv"
                              checkBudgetBalancedInv_prop)
              , (testProperty "addBudget" addBudgetValidInd_prop)
              ]
