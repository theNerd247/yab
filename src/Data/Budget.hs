{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}

{-|
Module      : BudgetName
Description : Budget datatype definitions
Copyright   : 
License     : GPL-2
Maintainer  : theNerd247
Stability   : experimental
Portability : POSIX

-}
module Data.Budget
    ( module Data.Amount
    , Budget(..)
    , BudgetData(..)
    , Entry(..)
    , BudgetName(..)
    , BudgetIndex(..)
    , checkBudgetBalanced
    , budgetBalance
    , addBudget
    , addBudgetData
    , budgetName
    , budgetAmount
    , entryDate
    , entryDesc
    , entryAmount
    , budgetTag
    , treeNode
    , budgetIncome
    , budgetExpenses
    ) where

import           Data.Amount
import           Data.Tree
import qualified Data.Map       as DM
import qualified Data.List      as DL
import           Control.Lens
import           Data.Tree.Lens
import           Data.Time

type BudgetName = String

data BudgetData = BudgetData { _budgetName   :: BudgetName
                             , _budgetAmount :: Amount
                             }
    deriving (Eq, Ord, Show)

makeLenses ''BudgetData

type Budget = Tree BudgetData

type BudgetIndex = Index Budget

data Entry = Entry { 
                   -- ^ The date of the entry
                   _entryDate   :: Day
                   , 
                   -- ^ The description of the entry
                   _entryDesc   :: String
                   , 
                   -- ^ The amount of the entry 
                   _entryAmount :: Double
                   , 
                   -- ^ the budget the entry belongs to
                   _budgetTag   :: Maybe BudgetName
                   }
    deriving (Show, Read, Eq)

makeLenses ''Entry

budgetIncome :: Lens' Budget Amount
budgetIncome = root . budgetAmount

-- | Checks if a budget is balanced (its income is equal to its spending)
checkBudgetBalanced :: Budget -> Bool
checkBudgetBalanced = (== 0) . budgetBalance

-- | Return the balance of a budget. This is simply the difference between the
-- budget's amount and the sum of the immediate sub-budgets amounts.
budgetBalance :: Budget -> Amount
budgetBalance budget
    | DL.null $ budget ^. branches = 0
    | otherwise = (budget ^. budgetIncome) -
          (sum $ budget ^. branches & each %~ view budgetIncome)

-- | @addBudget bdata bname budget@ adds BudgetData @bdata@ to the sub-budget
-- @bname@ found in @budget@. @Nothing@ is returned if @bname@ doesn't exist
addBudgetData :: BudgetData -> BudgetIndex -> Budget -> Budget
addBudgetData = addBudget . flip Node []

addBudget :: Budget -> BudgetIndex -> Budget -> Budget
addBudget new ind old = old & treeNode ind . branches %~ (new :)

budgetExpenses :: Budget -> Amount
budgetExpenses b = sum $ b ^.. branches . each . budgetIncome

treeNode :: (Index (Tree a)) -> Traversal' (Tree a) (Tree a)
treeNode [] = id
treeNode (x:xs) = branches . ix x . treeNode xs
