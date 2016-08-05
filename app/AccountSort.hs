{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}

{-|
Module      : Name
Description : AccountSort UI module
Copyright   : 
License     : GPL-2
Maintainer  : theNerd247
Stability   : experimental
Portability : POSIX


-}
module AccountSort ( sortTransactions ) where

import           YabCommon
import           Pretty
import           Budget

import           Data.Budget
import           Data.Serialization
import           Yab.AccountSort

import qualified Data.Map              as DM
import qualified Data.List             as DL

import           Data.Maybe            ( fromMaybe )
import           System.FilePath.Posix ( (</>) )
import           Data.List             ( (\\) )

data DescMapNameException = DescMapNameException [Name]
    deriving (Generic, Typeable)

instance Show DescMapNameException where
    show (DescMapNameException ns) =
        "Accounts do not exist in transaction map file at: "
            ++ (mconcat $ DL.intersperse "\n  " ns)

instance Exception DescMapNameException

sortTransactions :: (MonadCatch m, MonadIO m)
                 => FilePath
                 -> FilePath
                 -> Budget
                 -> m ()
sortTransactions tfpath bdir b = do
    transEntries <- liftIO $ loadCSVFile tfpath
    descMap <- liftIO $ readYamlFile (bdir </> "trans.yaml")
    checkValidMap descMap (budgetAccounts b)
    dm <- return $ mapTransactions (getTransEntry <$> transEntries) descMap
    liftIO $ putStrLn "Here's the sorted entries"
    liftIO $ putStrLn . prettyShow $ dm
    runUserInput "Do you want to save these results? (y/n)" $
        [ ("y", saveSortedEntries bdir dm b), ("n", return ()) ]
  where


saveSortedEntries :: (MonadCatch m, MonadIO m)
                  => FilePath
                  -> SortedEntries
                  -> Budget
                  -> m ()
saveSortedEntries budgetDir s b = do
    -- save Nothing entries to /tmp
    ofp <- otherFP
    saveCSVFile ofp $ nonSortedEntries s
    liftIO $ putStrLn $ "Saved non-sorted entries in: " ++ ofp
    finalEntries <- handleDuplicateEntries originalEntries (sortedEntries s)
    serialize (budgetDir </> "budget.yaml") $
        updateBudgetAccountsWith (updateEntries finalEntries) b
  where
    otherFP = today >>=
        \td -> return $ budgetDir </> ("unsorted-" ++ (show td) ++ ".csv")
    originalEntries = toEntriesMap $ budgetAccounts b

-- | checks if the description map is valid. If so then it does nothing.
-- Otherwise it throws an exception.
checkValidMap :: (MonadThrow m) => DescMap -> BudgetAccounts -> m ()
checkValidMap m bm
    | DL.length goodKeys == DM.size m =
          return ()
    | otherwise = throwM $ DescMapNameException (DM.keys m \\ goodKeys)
  where
    goodKeys = getMatchingKeys m bm

handleDuplicateEntries :: (MonadIO m)
                       => EntriesMap
                       -> EntriesMap
                       -> m EntriesMap
handleDuplicateEntries o s
    | DM.null duplicates = return nonDuplicatedEntries
    | otherwise = do
          liftIO $ putStrLn "The following entires would be duplicated: "
          liftIO . putStrLn $ prettyShow duplicates
          runUserInput "Would you like to save the duplicate entries? (y/n)" $
              [ ("y", return $ DM.unionWith (++) o s)
              , ("n", return $ nonDuplicatedEntries)
              ]
  where
    nonDuplicatedEntries = DM.unionWith DL.union o s
    duplicates = fst $
        DL.mapAccumL (\newS k -> (DM.update (checkDuplicates k) k newS, k))
                     s
                     (DM.keys s)
    checkDuplicates k se = maybe Nothing
                                 (\oes -> toNonEmpty $ DL.intersect oes se) $
        (DM.lookup k o)
    toNonEmpty [] = Nothing
    toNonEmpty l = Just l