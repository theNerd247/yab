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
  ,Name(..)
  ,Amount(..)
  ,Rate(..)
  ,Account(..)
  ,mergeAccounts
)
where

import YabCommon
import qualified Data.Map as DM
import Data.Budget.Entry

type Name = String

type Rate = Integer

type Amount = Double

data Account = Account
  {
  accountAmount :: Amount
  ,accountEntries :: Entries
  } deriving (Generic,Typeable,Show,Read,Eq)


data Budget = Budget
  {
    -- | The account rates
    budgetAccounts :: DM.Map Name Account
    -- | The total income rate of the budget (in $/budget period)
    ,budgetIncome  :: Amount
    -- | The amount of time for a single budget period (in days)
    ,budgetRate    :: Rate
  }
  deriving (Generic,Typeable,Show,Read,Eq)

-- | Merges two accounts. This is usefull for merging the data for accounts that
-- was parsed from a CSV and YAML files
mergeAccounts a b = Account 
  {
   accountAmount = accountAmount a
  ,accountEntries = (accountEntries a) ++ (accountEntries b)
  }

-- | Checks if a budget is balanced (its income is equal to its spending)
checkBudgetBalanced :: (MonadIO m) => Budget -> m ()
checkBudgetBalanced (Budget {budgetAccounts = as, budgetIncome = i}) = 
  let bal = i - (sum $ accountAmount <$> as)
  in case bal >= 0 of
    True -> liftIO $ putStrLn "Budget is balanced!" 
    False -> liftIO . putStrLn $ "Budget is unbalanced: " ++ (show bal)

-- | Checks the given account to make sure that it's balance isn't negative
checkAccount :: (MonadIO m) => Name -> Account -> m ()
checkAccount accName (Account {accountEntries = es, accountAmount = i}) = 
  let bal = Prelude.foldr ((+) . entryAmount) 0 es
  in case bal >= 0 of
    True -> liftIO . putStrLn $ "Account: " ++ accName ++ " is ok!"
    False -> liftIO . putStrLn $ "Account: " ++ accName ++ "is off! Min payoff: " 
      ++ (show $ findMinPayOff bal i)

findMinPayOff total rate
  | total >= 0 || rate == 0 = 0
  | otherwise = abs(total)/rate
