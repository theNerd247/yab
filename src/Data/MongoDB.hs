{-# LANGUAGE OverloadedStrings #-}

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
import qualified Data.Text        as DT

instance Val Budget where
    val b = Doc $
        [ "name" =: (budgetName bd)
        , "amount" =: (budgetAmount bd)
        , "subBudgets" =: subBudgets
        ]
      where
        bd = rootLabel b
        subBudgets = subForest b
    cast' (Doc d) = do
        name <- DB.lookup "name" d
        amount <- DB.lookup "amount" d
        subBudgets <- DB.lookup "subBudgets" d
        return $
            Node { rootLabel = BudgetData name amount, subForest = subBudgets }
    cast' _ = Nothing

-- | Run a mongo action in our database
runMongoCmd :: (MonadIO m) => Action m a -> m a
runMongoCmd db act = do
    pipe <- connect $ host "127.0.0.1"
    d <- access pipe master db act
    close pipe
    return d

-- | Fetch a budget with the given name from the database 
pullBudget db = runMongoCmd db . getBudget

-- | Save a budget to the server. 
pushBudget db = runMongoCmd db . putBudget

-- | MongoDB action to get a budget from the "budgets" collection.
getBudget :: (MonadIO m) => BudgetName -> Action m (Maybe Budget)
getBudget bName = liftM (>>= at $ DT.pack bName) . findOne $
    select [ "name" =: bName ] "budgets"

-- | MongoDB action to write a budget to the "budgets" collection.
putBudget :: (MonadIO m) => Budget -> Action m ()
putBudget = save "budgets" . (\Doc d -> d) . val
