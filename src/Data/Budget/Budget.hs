{-#LANGUAGE DeriveGeneric #-}
{-#LANGUAGE DeriveDataTypeable #-}
{-|
Module      : Name
Description : Budget datatype definitions
Copyright   : 
License     : GPL-2
Maintainer  : theNerd247
Stability   : experimental
Portability : POSIX

-}

module Data.Budget.Budget
(
  Budget(..)
  ,BudgetAccount(..)
  ,Name(..)
  ,Amount(..)
  ,Rate(..)
)
where

import YabCommon
import qualified Data.Map as DM

type BudgetAccount = DM.Map Name Amount

data Budget = Budget
  {
    -- | The account rates
    budgetAccounts :: BudgetAccount
    -- | The total income rate of the budget (in $/budget period)
    ,budgetIncome  :: Amount
    -- | The amount of time for a single budget period (in days)
    ,budgetRate    :: Rate
  }
  deriving (Generic,Typeable,Show,Read,Eq,Ord)
