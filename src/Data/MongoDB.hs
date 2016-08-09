{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

{-|
Module      : Data.MongoDB
Description : MongoDB interface
Copyright   :
License     : GPL-2
Maintainer  : theNerd247
Stability   :
Portability : POSIX

-}
module Data.MongoDB
    ( pushBudget
    , pullBudget
    ) where

import           Database.MongoDB
import           Data.Budget
import           Control.Monad
import           Data.Tree
import           Data.Tree.Lens
import           Control.Lens
import qualified Data.Text              as DT
import qualified Data.Bson              as DB
import           Control.Monad.IO.Class

instance Val Budget where
    val b = Doc $
        [ "name" =: (b ^. root . budgetName)
        , "amount" =: (b ^. root . budgetAmount)
        , "subBudgets" =: (b ^. branches)
        ]
    cast' (Doc d) = do
        name <- DB.lookup "name" d
        amount <- DB.lookup "amount" d
        subBudgets <- DB.lookup "subBudgets" d
        return $
            Node { rootLabel = BudgetData name amount, subForest = subBudgets }
    cast' _ = Nothing

-- | Run a mongo action in our database
runMongoCmd :: (MonadIO m) => Database -> Action m a -> m a
runMongoCmd db act = do
    pipe <- liftIO . connect $ host "127.0.0.1"
    d <- access pipe master db act
    liftIO $ close pipe
    return d

-- | Fetch a budget with the given name from the database 
pullBudget :: (MonadIO m) => Database -> BudgetName -> m (Maybe Budget)
pullBudget db = runMongoCmd db . getBudget

-- | Save a budget to the server. 
pushBudget :: (MonadIO m) => Database -> Budget -> m ()
pushBudget db = runMongoCmd db . putBudget

-- | MongoDB action to get a budget from the "budgets" collection.
getBudget :: (MonadIO m) => BudgetName -> Action m (Maybe Budget)
getBudget bName = liftM (>>= DB.at (DT.pack bName)) . findOne $
    select [ "name" =: bName ] "budgets"

-- | MongoDB action to write a budget to the "budgets" collection.
putBudget :: (MonadIO m) => Budget -> Action m ()
putBudget = save "budgets" . (\(Doc d) -> d) . val
