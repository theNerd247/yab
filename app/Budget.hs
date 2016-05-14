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
import Pretty

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

sortTransactions :: (MonadCatch m, MonadIO m) => FilePath -> FilePath -> Budget -> m ()
sortTransactions tfpath bdir b = do
  transEntries <- liftIO $ loadCSVFile tfpath
  descMap <- liftIO $ readYamlFile (bdir </> "trans.yaml")
  checkValidMap descMap (budgetAccounts b)
  dm <- return $ mapTransactions (getTransEntry <$> transEntries) descMap
  liftIO $ putStrLn "Here's the sorted entries" 
  liftIO $ putStrLn . prettyShow $ dm
  runUserInput "Do you want to save these results? (y/n)" $
    [
       ("y",saveSortedEntries bdir dm b)
      ,("n",return ())
    ]
  where

saveSortedEntries :: (MonadCatch m, MonadIO m) => FilePath -> SortedEntries -> Budget -> m ()
saveSortedEntries budgetDir s b = do
  -- save Nothing entries to /tmp
  ofp <- otherFP
  saveCSVFile ofp $ nonSortedEntries s
  liftIO $ putStrLn $ "Saved non-sorted entries in: " ++ ofp
  finalEntries <- handleDuplicateEntries originalEntries (sortedEntries s)
  serialize (budgetDir </> "budget.yaml") $ updateBudgetAccountsWith (updateEntries finalEntries) b
  where
    otherFP = today >>= \td -> return $ budgetDir </> ("unsorted-" ++ (show td) ++ ".csv")
    originalEntries = toEntriesMap $ budgetAccounts b

checkValidMap :: (MonadThrow m) => DescMap -> BudgetAccounts -> m ()
checkValidMap m bm 
  | b = return ()
  | otherwise = throwM $ DescMapNameException 
  where
    b = DMO.getAll . mconcat $ (\k -> DMO.All $ DM.member k bm) <$> (DM.keys m) 

handleDuplicateEntries :: (MonadIO m) => EntriesMap -> EntriesMap -> m EntriesMap
handleDuplicateEntries o s
  | DM.null duplicates = return nonDuplicatedEntries
  | otherwise = do
    liftIO $ putStrLn "The following entires would be duplicated: "
    liftIO . putStrLn $ prettyShow duplicates
    runUserInput "Would you like to save the duplicate entries? (y/n)" $
      [
        ("y",return $ DM.unionWith (++) o s)
      , ("n",return $ nonDuplicatedEntries)
      ]
    where
      nonDuplicatedEntries = DM.unionWith DL.union o s
      duplicates = fst $ DL.mapAccumL (\newS k -> (DM.update (checkDuplicates k) k newS, k)) s (DM.keys s)
      checkDuplicates k se = maybe Nothing (\oes -> toNonEmpty $ DL.intersect oes se) $ (DM.lookup k o)
      toNonEmpty [] = Nothing
      toNonEmpty l = Just l

runUserInput :: (MonadIO m) => String -> [(String, m a)] -> m a
runUserInput prompt acts = do
  liftIO $ putStrLn prompt
  ui <- liftIO getLine
  runUserAct ui acts
    where
      runUserAct _ [] = fail "No actions provided for runUserInput...blame the programmer"
      runUserAct i acts = maybe defaultAct snd $ DL.find ((i ==) . fst) acts
      defaultAct = runUserInput prompt acts
