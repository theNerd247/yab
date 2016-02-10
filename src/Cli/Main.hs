{-|
Module      : Name
Description : modules for running the cli interfaces
Copyright   : 
License     : GPL-2
Maintainer  : theNerd247
Stability   : experimental
Portability : POSIX

-}

module Cli.Main
(
  runProg
)
where

import YabCommon
import Data.Budget
import Data.Serialization
import Cli.Types
import Cli.Parser

import qualified System.Directory as SD
import qualified System.FilePath.Posix as SFP
import qualified Data.Map as DM
import qualified Control.Monad.Trans.State.Lazy as CMS

import System.FilePath.Posix ((</>))

getBudgetFilePath :: MainOpts -> FilePath
getBudgetFilePath = (</> "budget.yaml") . budgetFileDir

runProg :: IO ()
runProg = getProgOpts >>= mainProg

data CliState = CliState
  {
    budget :: Budget
  , mOpts :: MainOpts
  , budgetFilePath :: FilePath
  }

type CliM = CMS.StateT CliState IO

mainProg ops = do
  b <- deserialize bfp
  CMS.evalStateT (runCmd (prog ops)) (initState b)
  where
    mo = mainOpts ops
    bfp = getBudgetFilePath mo
    initState b = CliState 
      {
        budget = b
      , mOpts  = mo
      , budgetFilePath = bfp
      }

class Command a where
  runCmd :: a -> CliM ()

instance Command Prog where
  runCmd (AccountProg cmd) = runCmd cmd
  runCmd (BudgetProg cmd) = runCmd cmd

instance Command AccountCommand where
  runCmd (AddAccount ops) = do 
    runAccountCmd $ (saveAccounts =<<) . return . addAccount n (value ops)
    printOut $ "Added account: " ++ n
      where
        n = name ops
    
  runCmd (RemoveAccount ops) = do 
    runAccountCmd $ (saveAccounts =<<) . removeAccount ops
    printOut $ "Removed Account: " ++ ops

  runCmd (MergeAccounts ops) = do 
    runAccountCmd $ (saveAccounts =<<) . mergeAccounts accA accB
    printOut $ "Merged Accounts: " ++ accA ++ " " ++ accB
      where
        accA = accountA ops
        accB = accountB ops

  runCmd (AccountStatus ops) = 
       runAccountCmd $ (checkAccount ops =<<) . getAccount ops

instance Command BudgetCommand where
  runCmd BudgetStatus = runWithBudget $ \b -> checkBudgetBalanced b >> checkAccounts b
  runCmd (NewPayCheck ops) = undefined

getAccount :: (MonadThrow m) => Name -> BudgetAccounts -> m Account
getAccount name =  maybe (throwM $ NoSuchAccount name) return . DM.lookup name

runWithBudget :: (Budget -> CliM a) -> CliM a
runWithBudget f = CMS.gets budget >>= f

saveBudget :: CliM ()
saveBudget = do
    b <- CMS.gets budget
    bfp <- CMS.gets budgetFilePath
    serialize bfp b

modifyAccounts :: BudgetAccounts -> CliState -> CliState
modifyAccounts a s@(CliState{budget = b}) = s {budget = b {budgetAccounts = a}}

runAccountCmd :: (BudgetAccounts -> CliM a) -> CliM a
runAccountCmd f =
  runWithBudget (f . budgetAccounts)
  `catchAll` printEAndExit

saveAccounts :: BudgetAccounts -> CliM ()
saveAccounts accs = CMS.modify (modifyAccounts accs) >> saveBudget

printOut = liftIO . putStrLn
