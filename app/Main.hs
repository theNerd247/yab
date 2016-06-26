{-#LANGUAGE DeriveGeneric #-}
{-#LANGUAGE DeriveDataTypeable #-}

{-|
Module      : Name
Description : modules for running the cli interfaces
Copyright   : 
License     : GPL-2
Maintainer  : theNerd247
Stability   : experimental
Portability : POSIX

-}

module Main

where

import YabCommon
import Data.Budget
import Data.Serialization
import Types
import Parser
import Budget
import Pretty
import AccountSort

import qualified Data.Foldable as DF
import qualified Data.List as DL
import qualified System.Directory as SD
import qualified System.FilePath.Posix as SFP
import qualified Data.Map as DM
import qualified Control.Monad.Trans.State.Lazy as CMS

import System.FilePath.Posix ((</>))

data CliState = CliState
  {
   mOpts :: MainOpts
  }

data BudgetExistsException = BudgetExistsException deriving Typeable

instance Show BudgetExistsException where
  show BudgetExistsException = "budget.yaml already exists!"

instance Exception BudgetExistsException

type CliM = CMS.StateT CliState IO

class Command a where
  runCmd :: a -> CliM ()

instance Command Prog where
  runCmd (AccountProg cmd) = runCmd cmd
  runCmd (BudgetProg cmd) = runCmd cmd

instance Command AccountCommand where
  runCmd (AddAccount name amount) = do 
    runAccountCmdAndSave $ addAccount' name amount
    printOut $ "Added Account: " ++ name
    
  runCmd (RemoveAccount name) = do 
    runAccountCmdAndSave $ removeAccount name
    printOut $ "Removed Account: " ++ name

  runCmd (MergeAccounts accA accB) = do 
    runAccountCmdAndSave $ mergeAccounts accA accB
    printOut $ "Merged Accounts: " ++ accA ++ " " ++ accB

  runCmd (AccountStatus name) = 
    runAccountCmd $ (printAccountStatus name =<<) . getAccount name

  runCmd (ShowEntries name) =
    runAccountCmd $ (printAccountEntries =<<) . getAccount name

  runCmd (NewAccountEntry name entry) = 
    runAccountCmd $ saveAccounts . DM.adjust mkEntries name
      where
        mkEntries a = a {accountEntries = apnd entry . accountEntries $ a}
        apnd a l = l ++ [a]

  runCmd (SortTrans fpath) = runWithBudget $ \budget -> do
    fp <- getBudgetDir
    sortTransactions fpath fp budget

instance Command BudgetCommand where
  runCmd BudgetStatus = runWithBudget $ \b -> do 
    printBudgetBalanced b 
    printAccountStatuses b

  runCmd BudgetBalance = runWithBudget printBudgetBalance

  runCmd (NewPayCheck am da) = do
    b <- runWithBudget $ return . newPaycheck am da
    saveBudget b

  runCmd NewPayCheckAcc = do
    bdir <- getBudgetDir
    budget <- getBudget
    paycheckAcc <- loadCSVFile (newpayFile bdir)
    b <- DF.foldrM newpayacc budget paycheckAcc
    saveBudget b
    liftIO $ writeFile (newpayFile bdir) ""
    where
      newpayFile bdir = bdir </> "accounts" </> "newpay.csv"
      newpayacc e b = do 
        liftIO . putStrLn $ "Creating new paycheck from: " ++ (prettyShow e)
        return $ newPaycheck (entryAmount e) (entryDate e) b

  runCmd InitBudgetDir = do
    f <- getBudgetDir
    -- create the new budget or throw an error because the file exists
    b <- liftIO $ SD.doesFileExist (f </> "budget.yaml") >>= mkBudget
    liftIO $ SD.createDirectoryIfMissing False $ f </> "accounts"
    saveBudget b
    where
      mkBudget True = throwM BudgetExistsException
      mkBudget False = return $ Budget DM.empty 0 0

  runCmd ListAccounts = runWithBudget printAccountList

main = getProgOpts >>= \ops -> do
  CMS.evalStateT (runCmd (prog ops)) (initState ops)
  `catchAll`
  printEAndExit
  where
    initState o = CliState 
      {
       mOpts = mainOpts o
      }

getAccount :: (MonadThrow m) => Name -> BudgetAccounts -> m Account
getAccount name = maybe (throwM $ AccountNotExistsException name) return . DM.lookup name

runWithBudget :: (Budget -> CliM a) -> CliM a
runWithBudget f = getBudget >>= f

saveBudget :: Budget -> CliM ()
saveBudget b = getBudgetFilePath >>= flip serialize b

runAccountCmd :: (BudgetAccounts -> CliM a) -> CliM a
runAccountCmd f = runWithBudget (f . budgetAccounts)

runAccountCmdAndSave f = runWithBudget $ \b -> do
  a <- f (budgetAccounts b)
  saveBudget $ b {budgetAccounts = a}

saveAccounts :: BudgetAccounts -> CliM ()
saveAccounts accs = getBudgetDir >>= flip serialize accs

getBudget :: CliM Budget
getBudget = getBudgetFilePath >>= deserialize

getBudgetDir = CMS.gets mOpts >>= return . budgetFileDir
getBudgetFileName = CMS.gets mOpts >>= return . budgetFileName
getBudgetFilePath = do
  bfd <- getBudgetDir
  bfn <- getBudgetFileName
  return (bfd </> bfn)

printOut = liftIO . putStrLn
