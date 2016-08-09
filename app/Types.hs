{-# LANGUAGE TupleSections   #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE TemplateHaskell #-}

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
    , BudgetCommand(..)
    , mainOpts
    , prog
    , mOpsCurDB
    ) where

import           Data.Budget
import           Control.Lens
import           Database.MongoDB

import qualified Options.Applicative as OA

data MainProg = MainProg { _mainOpts :: MainOpts
                         , _prog     :: Prog
                         }

data MainOpts = MainOpts { _mOpsCurDB :: Database }

data Prog = BudgetProg BudgetCommand

data BudgetCommand = BudgetBalance BudgetName
                   | NewBudget BudgetName
                   | BudgetStatus BudgetName

makeLenses ''MainProg

makeLenses ''MainOpts
