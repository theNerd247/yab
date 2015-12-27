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
import Data.Budget.Entry

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



checkBudget :: (MonadIO m) => Budget -> m ()
checkBudget (Budget {budgetAccounts = as, income = i}) = 
  let bal = i - (sum as)
  in case bal >= 0 of
    True -> putStrLn "Budget is balanced!" 
    False -> putStrLn "Budget is unbalanced: " ++ (show bal)


-- | Checks the given account to make sure that it's balance isn't negative
checkAccount :: (MonadIO m) => Name -> Amount -> Entries -> m ()
checkAccount accName i es = 
  let bal = Prelude.foldr ((+) . entryAmount) 0 es
  in case bal >= 0 of
    True -> putStrLn $ "Account: " ++ accName ++ " is ok!"
    False -> putStrLn $ "Account: " ++ accName ++ "is off! Min payoff: " 
      ++ (show $ findMinPayOff bal i)

findMinPayOff total rate
  | total >= 0 || rate == 0 = 0
  | otherwise = abs(total)/rate
