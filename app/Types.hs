{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GADTs         #-}

{-|
Module      : Name
Description : 
Copyright   : (c) Some Guy, 2013
License     : GPL-3
Maintainer  : sample@email.com
Stability   : experimental
Portability : POSIX


-}
module Types
    ( MainProg(..)
    , MainOpts(..)
    , Prog(..)
    , AccountCommand(..)
    , BudgetCommand(..)
    ) where

import           YabCommon
import           Data.Budget
import           Control.Applicative

import qualified Options.Applicative as OA

data MainProg = MainProg { mainOpts :: MainOpts
                         , prog     :: Prog
                         }

data MainOpts = MainOpts { budgetFileDir  :: FilePath
                         , budgetFileName :: FilePath
                         }

data Prog = AccountProg AccountCommand
          | BudgetProg BudgetCommand

data BudgetCommand = BudgetStatus
                   | BudgetBalance
                   | NewPayCheck Amount Day
                   | NewPayCheckAcc
                   | ListAccounts
                   | InitBudgetDir

data AccountCommand = AddAccount Name Amount
                    | RemoveAccount Name
                    | MergeAccounts Name Name
                    | AccountStatus Name
                    | NewAccountEntry Name Entry
                    | ShowEntries Name
                    | SortTrans FilePath