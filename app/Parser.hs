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

module Parser
(
  getProgOpts
)
where

import YabCommon
import Data.Budget
import Data.Serialization
import Types
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

cmd' n h ops = OA.command n $ OA.info ops (OA.fullDesc <> OA.progDesc h)

instance Parseable MainProg where
  parse = MainProg <$> parse <*> parse

instance Parseable MainOpts where
  parse = MainOpts <$> pBudgetFileDir <*> pBudgetFileName

pBudgetFileName = OA.strOption $
     OA.value "budget.yml"
  <> OA.short 'f'
  <> OA.long "budget-file"
  <> OA.help "The budget YAML file to use"
  <> OA.metavar "BUDGETFILE"

pBudgetFileDir = OA.strOption $
     OA.value "."
  <> OA.short 'd'
  <> OA.long "budget-dir"
  <> OA.help "The budget directory to use"
  <> OA.metavar "BUDGETDIR"

instance Parseable Prog where
  parse = OA.hsubparser $
       cmd' "budget" "Commands to interface with the budget" (BudgetProg <$> parse) 
    <> cmd' "account" "Commands to interface with accounts" (AccountProg <$> parse)

class CommandGroup a where
  cmds :: [OA.Mod OA.CommandFields a]

  parseCmds :: OA.Parser a
  parseCmds = OA.hsubparser (mconcat cmds)

instance Parseable BudgetCommand where
  parse = parseCmds

instance CommandGroup BudgetCommand where
  cmds = 
    [
      cmd' "status" "get the overal status of the budget" $ pure BudgetStatus
     ,cmd' "newpay" "adds a new paycheck to the accounts" $ NewPayCheck <$> parse <*> parse
     ,cmd' "from-newpay" "Creates new paycheck entries based on entries in 'newpay' account" $ pure NewPayCheckAcc
     ,cmd' "ls" "list the accounts in the budget" $ pure ListAccounts 
     ,cmd' "init" "initialize the budget directory" $ pure InitBudgetDir
    ]

instance Parseable AccountCommand where
  parse = parseCmds

instance CommandGroup AccountCommand where
  cmds = 
    [
      cmd' "add" "add a new account" $ AddAccount <$> parseName <*> parse
     ,cmd' "rm" "remove an account" $ RemoveAccount <$> parseName 
     ,cmd' "merge" "merges two accounts into the first given" $ MergeAccounts <$> parseName <*> parseName
     ,cmd' "status" "the status of an account" $ AccountStatus <$> parseName
     ,cmd' "new-entry" "add a new entry to the account" $ NewAccountEntry <$> parseName <*> parse
     ,cmd' "show" "show the account's entries" $ ShowEntries <$> parseName
     ,cmd' "sort" "Sort entries in given bank transaction file to respective accounts" 
       $ SortTrans <$> parseString "FILEPATH" "File path to csv bank transaction file"
    ]

instance Parseable Amount where
  parse = OA.argument OA.auto $ 
    OA.help "A dollar amount as a floating point number"
    <> OA.metavar "AMNT"

instance Parseable Day where
  parse = OA.argument getDay $ 
      OA.help "A day in the format mm/dd/yy"
      <> OA.metavar "DAY"
    where
      -- use our parser to parse the date
      getDay = OA.str >>= parseDate

instance Parseable Entry where
  parse = Entry <$> parse <*> desc <*> parse
    where
      desc = parseString "DESC" "The description of the entry"

parseString meta h = OA.strArgument $ OA.help h <> OA.metavar meta
parseName = parseString "NAME" "The name of the account"
