{-|
Module      : Name
Description : functions for running cli commands with the budget
Copyright   : 
License     : GPL-2
Maintainer  : theNerd247
Stability   : experimental
Portability : POSIX


-}

module Cli.Budget
(
  printAccountStatus
  ,printBudgetBalanced
  ,printAccountStatuses
  ,printAccountList
  ,printAccountEntries
)
where

import YabCommon
import Data.Budget
import Data.Serialization
import qualified Data.Map as DM
import qualified System.Directory as SD

import System.FilePath.Posix ((</>))

printBudgetBalanced b = 
  case checkBudgetBalanced b of
    True -> liftIO $ putStrLn "Budget is balanced!" 
    False -> liftIO . putStrLn $ "Budget is unbalanced: " ++ (showAmount . budgetBalance $ b)

printAccountStatuses :: (MonadIO m) => Budget -> m ()
printAccountStatuses = sequence_ . DM.elems . DM.mapWithKey printAccountStatus . budgetAccounts

printAccountStatus accName a@(Account {accountAmount = i}) = case checkAccount a of
  True -> liftIO . putStrLn $ "Account: " ++ accName ++ " is ok! " ++ (showAmount bal)
  False -> liftIO . putStrLn $ 
    "Account: " ++ accName ++ " is off! Min payoff: " ++ (showDecimal $ findMinPayOff bal i)
  where
    bal = accountBalance a

printAccountList :: (MonadIO m) => Budget -> m ()
printAccountList = liftIO . mapM_ putStrLn . DM.keys . budgetAccounts

printAccountEntries :: (MonadIO m) => Account -> m () 
printAccountEntries = liftIO . mapM_ (putStrLn . printEntry) . accountEntries
  where
    printEntry (Entry da de a) = (show da) ++ " " ++ (show $ take 40 de) ++ " " ++ (showAmount a)
