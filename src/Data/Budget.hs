{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveDataTypeable #-}

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
    , BudgetData
    , Entry(..)
    , checkBudgetBalanced
    , budgetBalance
    , addBudget
    , addBudgetData
    ) where

import           Data.Amount
import           Data.Tree
import qualified Data.Map     as DM
import qualified Data.List    as DL
import           Control.Lens

type BudgetName = String

type Rate = Integer

data BudgetData = BudgetData { _budgetName   :: BudgetName
                             , _budgetAmount :: Amount
                             }
    deriving (Eq, Ord, Show)

makeLenses ''BudgetData

type Budget = Tree BudgetData

type BudgetIndex = IxValue Budget

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
    deriving (Generic, Typeable, Show, Read, Eq)

makeLenses ''Entry

-- | Checks if a budget is balanced (its income is equal to its spending)
checkBudgetBalanced :: Budget -> Bool
checkBudgetBalanced = (== 0) . budgetBalance

-- | Return the balance of a budget. This is simply the difference between the
-- budget's amount and the sum of the immediate sub-budgets amounts.
budgetBalance :: Budget -> Amount
budgetBalance budget
    | DL.null subBudgets = 0
    | otherwise = (budget ^. budgetIncome) -
          (sum $ budget ^. branches . budgetIncome)

-- | @addBudget bdata bname budget@ adds BudgetData @bdata@ to the sub-budget
-- @bname@ found in @budget@. @Nothing@ is returned if @bname@ doesn't exist
addBudgetData :: BudgetData -> BudgetIndex -> Budget -> Budget
addBudgetData bdata ind budget =
    addBudget . flip Node []

addBudget :: Budget -> BudgetIndex -> Budget -> Budget
addBudget new ind old = old & branches . ix ind %~ (new :)
