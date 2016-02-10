{-#LANGUAGE TupleSections #-}
{-#LANGUAGE GADTs #-}

{-|
Module      : Name
Description : 
Copyright   : (c) Some Guy, 2013
License     : GPL-3
Maintainer  : sample@email.com
Stability   : experimental
Portability : POSIX


-}

module Cli.Types
(
  MainProg(..)
  ,MainOpts(..)
  ,Prog(..)
  ,AccountCommand(..)
  ,BudgetCommand(..)
  ,AddAccountOpts(..)
  ,MergeAccountsOpts(..)
  ,NewPaycheckOpts(..)
  )
    where

import YabCommon
import Data.Budget
import Control.Applicative

import qualified Options.Applicative as OA

data MainProg = MainProg
  {
    mainOpts :: MainOpts
  , prog     :: Prog
  }

data MainOpts = MainOpts 
  {
    budgetFileDir :: FilePath
  }

data Prog = 
    AccountProg AccountCommand
  | BudgetProg BudgetCommand

data BudgetCommand = 
    BudgetStatus
  | NewPayCheck NewPaycheckOpts

data NewPaycheckOpts = NewPaycheckOpts
  {
    amount :: Maybe Amount
  , date  :: Maybe Day
  }

data AccountCommand = 
    AddAccount AddAccountOpts
  | RemoveAccount Name
  | MergeAccounts MergeAccountsOpts
  | AccountStatus Name

data AddAccountOpts = AddAccountOpts
  {
    name  :: Name
  , value :: Amount
  }

data MergeAccountsOpts = MergeAccountsOpts 
  {
    accountA  :: Name
  , accountB  :: Name
  }
