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
    ( Budget(..)
    , BudgetName(..)
    , Amount(..)
    , Rate(..)
    , Entry(..)
    , Entries(..)
    , mergeAccountData
    , checkBudgetBalanced
    , checkAccounts
    , checkAccount
    , addAccount
    , addAccount'
    , removeAccount
    , mergeAccounts
    , showAmount
    , showDecimal
    , accountBalance
    , budgetBalance
    , findMinPayOff
    , newPaycheck
    , updateBudgetAccountsWith
    ) where

import           Data.Tree
import qualified Data.Map  as DM
import qualified Data.List as DL

type BudgetName = String

type Rate = Integer

type Amount = Double

type BudgetName = String

type Amount = Double

data BudgetData = BudgetData { budgetBudgetName :: BudgetName
                             , budgetAmount     :: Amount
                             }
    deriving (Eq, Ord, Show)

type Budget = Tree BudgetData

data Entry = Entry { 
                   -- | The date of the entry
                   entryDate   :: Day
                   , 
                   -- | The description of the entry
                   entryDesc   :: String
                   , 
                   -- | The amount of the entry 
                   entryAmount :: Double
                   , 
                   -- | the budget the entry belongs to
                   budgetTag   :: Maybe BudgetName
                   }
    deriving (Generic, Typeable, Show, Read, Eq)

-- | Checks if a budget is balanced (its income is equal to its spending)
checkBudgetBalanced :: Budget -> Bool
checkBudgetBalanced = (== 0) . budgetBalance

budgetBalance :: Budget -> Amount
budgetBalance (Budget{budgetAccounts = as,budgetIncome = i})
    | DM.null as = 0
    | otherwise = i - (sum $ accountAmount <$> as)

-- | Checks the accounts contained within a budget using @checkAccount@
checkAccounts :: Budget -> DM.Map BudgetName Bool
checkAccounts = fmap checkAccount . budgetAccounts

-- | Checks the given account to make sure that it's balance isn't negative
checkAccount :: Account -> Bool
checkAccount = (>= 0) . accountBalance

accountBalance (Account{accountEntries = es,accountAmount = i}) =
    Prelude.foldr ((+) . entryAmount) 0 es

showAmount :: (Show a) => a -> String
showAmount = ("$" ++) . showDecimal

showDecimal :: (Show a) => a -> String
showDecimal d = maybe s (flip take s . (+ 3)) $ i
  where
    s = show d
    i = DL.elemIndex '.' s

-- | Merges two accounts. This is usefull for merging the data for accounts that
-- was parsed from a CSV and YAML files. The accountAmount value is taken from
-- the first account given. The entries are combined with a union.
mergeAccountData a b = Account { accountAmount = accountAmount a +
                                   accountAmount b
                               , accountEntries = DL.union (accountEntries a)
                                                           (accountEntries b)
                               }

updateBudgetAccountsWith :: (BudgetAccounts -> BudgetAccounts)
                         -> Budget
                         -> Budget
updateBudgetAccountsWith f b =
    b { budgetAccounts = f (budgetAccounts b) }

addAccount :: BudgetName -> Amount -> BudgetAccounts -> BudgetAccounts
addAccount n a = DM.insert n (Account a [])

addAccount' :: (MonadThrow m)
            => BudgetName
            -> Amount
            -> BudgetAccounts
            -> m BudgetAccounts
addAccount' n a b
    | DM.member n b = throwM $ AccountExistsException n
    | otherwise = return $ addAccount n a b

-- | Remove an account from the budget. If the account does exist then AccountNotExistsException is thrown
removeAccount :: (MonadThrow m)
              => BudgetName
              -> BudgetAccounts
              -> m BudgetAccounts
removeAccount n m
    | DM.member n m == False =
          throwM $ AccountNotExistsException n
    | otherwise = return $ DM.delete n m

-- | Merge two accounts. IF either account doesn't exist then AccountNotExistsException is thrown.
mergeAccounts :: (MonadThrow m)
              => BudgetName
              -> BudgetName
              -> BudgetAccounts
              -> m BudgetAccounts
mergeAccounts na nb bas = do
    accA <- checkFail (DM.lookup na bas) na
    accB <- checkFail (DM.lookup nb bas) nb
    b <- removeAccount nb bas
    return $ DM.adjust (\_ -> mergeAccountData accA accB) na b
  where
    checkFail m n = maybe (throwM $ AccountNotExistsException n) return m

newDefaultPaycheck :: (MonadIO m) => Budget -> m Budget
newDefaultPaycheck b@(Budget{budgetAccounts = ba,budgetIncome = bi}) =
    today >>= (\t -> return $ newPaycheck bi t b)

newPaycheck :: Amount -> Day -> Budget -> Budget
newPaycheck a d b = b { budgetAccounts = newPaycheckAccount <$> (budgetAccounts b)
                      }
  where
    newPaycheckAccount a@(Account{accountAmount = am,accountEntries = ens}) =
        a { accountEntries = (ens ++ [ newPaycheckEntry am ]) }
    newPaycheckEntry aa = Entry { entryDate = d
                                , entryDesc = "new paycheck"
                                , entryAmount = aa * paycheckRatio
                                }
    paycheckRatio = a / (budgetIncome b)

findMinPayOff total rate
    | total >= 0 || rate == 0 =
          0
    | otherwise = abs (total) / rate