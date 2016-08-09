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
    ( printBudgetBalanced
    , printBudgetBalance
    , runUserInput
    ) where

import           Data.Budget
import           Control.Monad.IO.Class

import qualified Data.List              as DL

printBudgetBalanced b = case checkBudgetBalanced b of
    True -> liftIO $ putStrLn "Budget is balanced!"
    False -> liftIO . putStrLn $
        "Budget is unbalanced: " ++ (showAmount . budgetBalance $ b)

printBudgetBalance b = liftIO . putStrLn $
    "Budget Balance: " ++ (showAmount . budgetBalance $ b)

runUserInput :: (MonadIO m) => String -> [(String, m a)] -> m a
runUserInput prompt acts = do
    liftIO $ putStrLn prompt
    ui <- liftIO getLine
    runUserAct ui acts
  where
    runUserAct _ [] = fail "No actions provided for runUserInput...blame the programmer"
    runUserAct i acts = maybe defaultAct snd $ DL.find ((i ==) . fst) acts
    defaultAct = runUserInput prompt acts
