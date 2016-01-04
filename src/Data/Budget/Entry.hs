{-#LANGUAGE DeriveGeneric #-}

{-|
Module      : Name
Description : Entry Data type declarations
Copyright   : 
License     : GPL-2
Maintainer  : theNerd247
Stability   : experimental
Portability : POSIX

-}

module Data.Budget.Entry
(
  Entry(..)
  ,Entries(..)
  ,filterEntries
)
where

import YabCommon
import Data.Budget.Day

import qualified Data.List as DL
import qualified Data.Map.Lazy as DML

-- | A single entry in an account file
data Entry = Entry
  {
    -- | The date of the entry
    entryDate   :: Day
    -- | The description of the entry
    ,entryDesc   :: String
    -- | The amount of the entry 
    ,entryAmount :: Double
  }
  deriving (Generic,Typeable,Show,Read,Eq)

type Entries = [Entry]

filterEntries = DL.filter
