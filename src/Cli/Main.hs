{-|
Module      : Cli.Main
Description : Cli main interface module
Copyright   : 
License     : GPL-2
Maintainer  : 
Stability   : experimental
Portability : POSIX

-}

module Cli.Main
(
)
where

import YabCommon
import Data.Budget
import Control.Applicative

import qualified Options.Applicative as OA

import Options.Applicative ((<>),idm)

data MainProg = MainProg
  {
    mainOpts :: MainOpts
    ,prog    :: Prog
  }

data MainOpts = MainOpts 
  {
   budgetFile :: FilePath
  }

data Prog = 
    AccountProg AccountCommand
  | BudgetProg BudgetCommand

data AccountCommand = 
    AddAccount AddAccountOpts
  | RemoveAccount RemoveAccountOpts
  | MergeAccounts MergeAccountsOpts

data BudgetCommand = 
    BudgetStatus
  | NewPayCheck NewPaycheckOpts

type AccountNameOpt = Name

type AddAccountOpts = AccountNameOpt 

type RemoveAccountOpts = AccountNameOpt

data MergeAccountsOpts = MergeAccountsOpts 
  {
   accountA :: AccountNameOpt
  ,accountB :: AccountNameOpt
  }

data NewPaycheckOpts = NewPaycheckOpts
  {
    amount :: Maybe Amount
    ,date  :: Maybe Day
  }

mainParser :: OA.Parser MainProg
mainParser = MainProg 
  <$> pMainOpts
  <*> pProgs

pMainOpts :: OA.Parser MainOpts
pMainOpts = MainOpts <$> pBudgetFile

pProgs :: OA.Parser Prog
pProgs = OA.subparser $
     pAccountProg 
  <> pBudgetProg

pBudgetFile = OA.strOption $ 
     OA.value "budget.yaml"
  <> OA.short 'b'
  <> OA.long "budget"
  <> OA.help "The budget config file to use"
  <> OA.metavar "BUDGETFILE"

cmd' n h ops = OA.command n $ OA.info ops (OA.progDesc h)
commandGroup p n h cmds = cmd' n h (p <$> (OA.subparser $ mconcat cmds))

pBudgetProg = commandGroup BudgetProg "budget" "General budget commands" 
  [
    pBudgetStatus
    ,pNewPaycheck
  ]

pAccountProg = commandGroup AccountProg "account" "General account commands"
  [
     pAddAccount
    ,pRemoveAccount
    ,pMergeAccounts
  ]

pBudgetStatus = cmd' "status" "get the overal status of the budget" (pure BudgetStatus)

pNewPaycheck :: OA.Mod OA.CommandFields BudgetCommand
pNewPaycheck = cmd' "newpay" "adds a new paycheck to the accounts based on the given budget" $ 
  NewPayCheck <$> (NewPaycheckOpts <$> optional pAmount <*> optional pDay)

pAddAccount = cmd' "add" "add a new account" $ 
  AddAccount <$> pAccountNameOpt 

pRemoveAccount = cmd' "rm" "remove an account" $
  RemoveAccount <$> pAccountNameOpt 

pMergeAccounts = cmd' "merge" "merges two accounts" $ 
  MergeAccounts <$> (MergeAccountsOpts <$> pAccountNameOpt <*> pAccountNameOpt)

pAmount :: OA.Parser Amount
pAmount = OA.argument OA.auto $ OA.help "A dollar amount as a floating point number"

pDay :: OA.Parser Day
pDay = OA.argument OA.auto $ OA.help "A day in the format mm/dd/yy"

pAccountNameOpt :: OA.Parser Name
pAccountNameOpt = OA.strArgument $ OA.help "the name of an account defined in the budget file."
