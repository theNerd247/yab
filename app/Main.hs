{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}

{-|
Module      : Name
Description : modules for running the cli interfaces
Copyright   : 
License     : GPL-2
Maintainer  : theNerd247
Stability   : experimental
Portability : POSIX

-}
module Main where

import           Data.Budget
import           Data.MongoDB
import           Database.MongoDB               ( Database )
import           Types
import           Parser
import           Budget
import           Pretty
import           Control.Exception.Base
import           Control.Monad.Catch
import           Control.Lens
import qualified Control.Monad.Trans.State.Lazy as CMS
import qualified Data.Text                      as DT

data CliState = CliState { _mOpts :: MainOpts }

makeLenses ''CliState

data BudgetNotFound = BudgetNotFound { _bnfDB    :: Database
                                     , _bnfBName :: BudgetName
                                     }

makeLenses ''BudgetNotFound

instance Show BudgetNotFound where
    show bnf = "Budget " ++
        (bnf ^. bnfBName) ++ " in database " ++ (DT.unpack $ bnf ^. bnfDB)

instance Exception BudgetNotFound

type CliM = CMS.StateT CliState IO

class Command a where
    runCmd :: a -> CliM ()

instance Command Prog where
    runCmd (BudgetProg cmd) =
        runCmd cmd

instance Command BudgetCommand where
    runCmd (BudgetStatus bname) =
        runWithBudget bname $
            \b -> do
                printBudgetBalanced b
                printBudgetBalance b

    runCmd (BudgetBalance bname) =
        runWithBudget bname printBudgetBalance

runWithBudget :: BudgetName -> (Budget -> CliM a) -> CliM a
runWithBudget bName f = getBudget bName >>= f

saveBudget :: Budget -> CliM ()
saveBudget b = do
    db <- use currentDB
    pushBudget db b

currentDB = mOpts . mOpsCurDB

getBudget :: BudgetName -> CliM Budget
getBudget bname = do
    db <- use currentDB
    b <- pullBudget db bname
    maybe (throw $ BudgetNotFound db bname) return b

main = do
    ops <- getProgOpts
    CMS.evalStateT (runCmd (ops ^. prog)) (CliState $ ops ^. mainOpts)
    `catchAll` (putStrLn . show)
