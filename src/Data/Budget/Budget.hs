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
  ,BudgetAccounts(..)
  ,mergeAccountData
  ,checkBudgetBalanced
  ,checkAccounts
  ,addAccount
  ,removeAccount
  ,mergeAccounts
)
where

import YabCommon
import qualified Data.Map as DM
import qualified Data.List as DL
import Data.Budget.Entry

type Name = String

type Rate = Integer

type Amount = Double

data Account = Account
  { 
    accountAmount :: Amount
  , accountEntries :: Entries
  } deriving (Generic,Typeable,Show,Read,Eq)

type BudgetAccounts = DM.Map Name Account

data Budget = Budget
  {
    -- | The account rates
    budgetAccounts :: BudgetAccounts
    -- | The total income rate of the budget (in $/budget period)
    ,budgetIncome  :: Amount
    -- | The amount of time for a single budget period (in days)
    ,budgetRate    :: Rate
  }
  deriving (Generic,Typeable,Show,Read,Eq)

-- | Checks if a budget is balanced (its income is equal to its spending)
checkBudgetBalanced :: (MonadIO m) => Budget -> m ()
checkBudgetBalanced (Budget {budgetAccounts = as, budgetIncome = i}) = 
  let bal = i - (sum $ accountAmount <$> as)
  in case bal >= 0 of
    True -> liftIO $ putStrLn "Budget is balanced!" 
    False -> liftIO . putStrLn $ "Budget is unbalanced: " ++ (show bal)

checkAccounts :: (MonadIO m) => Budget -> m ()
checkAccounts = sequence_ . fmap runCheckAccount . DM.toList . budgetAccounts
  where
    runCheckAccount (n,a) = checkAccount n a

-- | Checks the given account to make sure that it's balance isn't negative
checkAccount :: (MonadIO m) => Name -> Account -> m ()
checkAccount accName (Account {accountEntries = es, accountAmount = i}) = 
  let bal = Prelude.foldr ((+) . entryAmount) 0 es
  in case bal >= 0 of
    True -> liftIO . putStrLn $ "Account: " ++ accName ++ " is ok!"
    False -> liftIO . putStrLn $ "Account: " ++ accName ++ "is off! Min payoff: " 
      ++ (show $ findMinPayOff bal i)

-- | Merges two accounts. This is usefull for merging the data for accounts that
-- was parsed from a CSV and YAML files. The accountAmount value is taken from
-- the first account given. The entries are combined with a union.
mergeAccountData a b = Account 
  {
   accountAmount = accountAmount a + accountAmount b
  ,accountEntries = DL.union (accountEntries a) (accountEntries b)
  }

data NoSuchAccount = NoSuchAccount Name deriving (Generic,Eq,Ord,Typeable)

instance Show NoSuchAccount where
  show (NoSuchAccount n) = "Account doesn't exist: " ++ n

instance Exception NoSuchAccount

addAccount :: Name -> Amount -> BudgetAccounts -> BudgetAccounts
addAccount n a = DM.insert n (Account a [])

-- | Remove an account from the budget. If the account does exist then NoSuchAccount is thrown
removeAccount :: (MonadThrow m) => Name -> BudgetAccounts -> m BudgetAccounts
removeAccount n m
  | DM.member n m == False = throwM $ NoSuchAccount n
  | otherwise = return $ DM.delete n m

-- | Merge two accounts. IF either account doesn't exist then NoSuchAccount is thrown.
mergeAccounts :: (MonadThrow m) => Name -> Name -> BudgetAccounts -> m BudgetAccounts
mergeAccounts na nb bas = do 
  accA <- checkFail (DM.lookup na bas) na
  accB <- checkFail (DM.lookup nb bas) nb
  b <- removeAccount nb bas
  return $ DM.adjust (\_ -> mergeAccountData accA accB) na b
  where 
    checkFail m n = maybe (throwM $ NoSuchAccount n) return m

findMinPayOff total rate
  | total >= 0 || rate == 0 = 0
  | otherwise = abs(total)/rate
