{-#LANGUAGE TypeSynonymInstances #-}
{-#LANGUAGE FlexibleInstances #-}

{-|
Module      : Cli.Main
Description : Cli main interface module
Copyright   : 
License     : GPL-2
Maintainer  : 
Stability   : experimental
Portability : POSIX

-}

module Cli.Parser
(
  getProgOpts
)
where

import YabCommon
import Data.Budget
import Cli.Types
import Control.Applicative

import qualified Options.Applicative as OA

import Options.Applicative ((<>),idm)

class Parseable a where
  parse :: OA.Parser a

getProgOpts :: (MonadIO m) => m MainProg
getProgOpts = liftIO $ OA.customExecParser prefs opts
  where
    opts = OA.info (OA.helper <*> parse)
      (
         OA.fullDesc
      <> OA.header "Yet Another Budgeting Program"
      )
    prefs = OA.prefs $ OA.showHelpOnError

cmd' n h ops = OA.command n $ OA.info ops (OA.progDesc h)

instance Parseable MainProg where
  parse = MainProg <$> parse <*> parse

instance Parseable MainOpts where
  parse = MainOpts <$> pBudgetFile

pBudgetFile = OA.strOption $ 
     OA.value "."
  <> OA.short 'b'
  <> OA.long "budget"
  <> OA.help "The budget config file to use"
  <> OA.metavar "BUDGETFILE"

instance Parseable Prog where
  parse = OA.subparser $
       cmd' "budget" "Commands to interface with the budget" (BudgetProg <$> parse) 
    <> cmd' "account" "Commands to interface with accounts" (AccountProg <$> parse)

class CommandGroup a where
  cmds :: [OA.Mod OA.CommandFields a]

  parseCmds :: OA.Parser a
  parseCmds = OA.subparser (mconcat cmds)

instance Parseable BudgetCommand where
  parse = parseCmds

instance CommandGroup BudgetCommand where
  cmds = 
    [
      cmd' "status" "get the overal status of the budget" (pure BudgetStatus)
     ,cmd' "newpay" "adds a new paycheck to the accounts based on the given budget" $ NewPayCheck <$> parse
    ]

instance Parseable NewPaycheckOpts where
  parse = NewPaycheckOpts <$> optional parse <*> optional parse

instance Parseable AccountCommand where
  parse = parseCmds

instance CommandGroup AccountCommand where
  cmds = 
    [
      cmd' "add" "add a new account" $ AddAccount <$> parse
     ,cmd' "rm" "remove an account" $ RemoveAccount <$> parse 
     ,cmd' "merge" "merges two accounts" $ MergeAccounts <$> parse
     ,cmd' "status" "the status of an account" $ AccountStatus <$> parse
    ]

instance Parseable AddAccountOpts where
  parse = AddAccountOpts <$> parse <*> parse

instance Parseable MergeAccountsOpts where
  parse = MergeAccountsOpts <$> parse <*> parse

instance Parseable Amount where
  parse = OA.argument OA.auto $ OA.help "A dollar amount as a floating point number"

instance Parseable Day where
  parse = OA.argument OA.auto $ OA.help "A day in the format mm/dd/yy"

instance Parseable Name where
  parse = OA.strArgument $ OA.help "the name of an account defined in the budget file."
