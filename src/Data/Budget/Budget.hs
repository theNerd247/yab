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
  ,NoSuchAccount(..)
  ,mergeAccountData
  ,checkBudgetBalanced
  ,checkAccounts
  ,checkAccount
  ,addAccount
  ,removeAccount
  ,mergeAccounts
  ,showAmount
  ,showDecimal
  ,accountBalance
  ,budgetBalance
  ,findMinPayOff
  ,newPaycheck
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

data NoSuchAccount = NoSuchAccount Name deriving (Generic,Eq,Ord,Typeable)

instance Show NoSuchAccount where
  show (NoSuchAccount n) = "Account doesn't exist: " ++ n

instance Exception NoSuchAccount

-- | Checks if a budget is balanced (its income is equal to its spending)
checkBudgetBalanced :: Budget -> Bool
checkBudgetBalanced = (>= 0) . budgetBalance

budgetBalance :: Budget -> Amount
budgetBalance (Budget {budgetAccounts = as, budgetIncome = i}) = 
  i - (sum $ accountAmount <$> as)

-- | Checks the accounts contained within a budget using @checkAccount@
checkAccounts :: Budget -> DM.Map Name Bool
checkAccounts = fmap checkAccount . budgetAccounts

-- | Checks the given account to make sure that it's balance isn't negative
checkAccount :: Account -> Bool
checkAccount = (>=0) . accountBalance 

accountBalance (Account {accountEntries = es, accountAmount = i}) = 
  Prelude.foldr ((+) . entryAmount) 0 es

showAmount :: (Show a) => a -> String
showAmount = ("$"++) . showDecimal

showDecimal :: (Show a) => a -> String
showDecimal d = maybe s (flip take s . (+3)) $ i
  where 
    s = show d
    i = DL.elemIndex '.' s

-- | Merges two accounts. This is usefull for merging the data for accounts that
-- was parsed from a CSV and YAML files. The accountAmount value is taken from
-- the first account given. The entries are combined with a union.
mergeAccountData a b = Account 
  {
   accountAmount = accountAmount a + accountAmount b
  ,accountEntries = DL.union (accountEntries a) (accountEntries b)
  }

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

newDefaultPaycheck :: (MonadIO m) => Budget -> m Budget
newDefaultPaycheck b@(Budget{
                     budgetAccounts = ba
                     , budgetIncome = bi
                     }) = today >>= (\t -> return $ newPaycheck bi t b)

newPaycheck :: Amount -> Day -> Budget -> Budget
newPaycheck a d b = b{budgetAccounts = newPaycheckAccount <$> (budgetAccounts b)}
  where
    newPaycheckAccount a@(Account{accountAmount = am, accountEntries = ens}) 
      = a{accountEntries = (ens ++ [newPaycheckEntry am])}
    newPaycheckEntry aa 
      = Entry {entryDate = d, entryDesc = "new paycheck", entryAmount = aa*paycheckRatio}
    paycheckRatio = a / (budgetIncome b)

findMinPayOff total rate
  | total >= 0 || rate == 0 = 0
  | otherwise = abs(total)/rate
