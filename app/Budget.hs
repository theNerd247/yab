{-|
Module      : Name
Description : functions for running cli commands with the budget
Copyright   : 
License     : GPL-2
Maintainer  : theNerd247
Stability   : experimental
Portability : POSIX


-}

module Budget
(
  printAccountStatus
  ,printBudgetBalanced
  ,printAccountStatuses
  ,printAccountList
  ,printAccountEntries
  ,sortTransactions
)
where

import YabCommon
import Data.Budget
import Data.Serialization
import AccountSort
import qualified Data.Map as DM
import qualified Data.List as DL
import qualified System.Directory as SD
import Data.Maybe (fromMaybe)

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

sortTransactions :: (MonadIO m) => FilePath -> FilePath -> Budget -> m ()
sortTransactions tfpath bdir b = do
  dm <- mapTransactionsFromFile tfpath (bdir </> "trans.yaml")
  liftIO $ putStrLn "Here's the sorted entries" 
  printSortedEntries dm
  runUserInput "Do you want to save these results? (y/n)" $
    [
       ("y",saveSortedEntries bdir dm b)
      ,("n",return ())
    ]

saveSortedEntries :: (MonadIO m) => FilePath -> SortedEntries -> Budget -> m ()
saveSortedEntries budgetDir sortedEntries b = do
  -- save Nothing entries to /tmp
  ofp <- otherFP
  writeCSVFile ofp otherEntries
  liftIO $ putStrLn "Saved non-sorted entries in: " ++ ofp
  serialize (budgetDir </> "budget.yaml") $ updateBudgetAccounts (updateEntries sortedEntryMap) b
  handleDuplicateEntries
  where
    otherEntries = DM.findWithDefault [] Nothing sortedEntries
    otherFP = today >>= \td -> return $ budgetDir </> ("unsorted-" ++ (show td) ++ ".csv")
    sortedEntryMap = DM.mapKeys fromMaybe $ DM.delete Nothing sortedEntries

handleDuplicateEntries :: (MonadIO m) => DM.Map Name Entries -> BudgetAccounts -> m ()
handleDuplicateEntries sorted ba
  | DM.empty duplicates == True = return ()
  | otherwise = do
    print

runUserInput :: (MonadIO m) => String -> [(String,m ())] -> m ()
runUserInput prompt acts = do
  liftIO $ putStrLn prompt
  ui <- liftIO getLine
  runUserAct ui acts
    where
      runUserAct _ [] = return ()
      runUserAct i acts = maybe defaultAct snd $ DL.find ((i ==) . fst) acts
      defaultAct = runUserInput prompt acts
