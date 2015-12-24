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
import qualified Data.List as DL
import qualified Data.Time as DT
import qualified Data.Map.Lazy as DML

-- | A single entry in an account file
data Entry = Entry
  {
    -- | The date of the entry
    entryDate   :: DT.Day
    -- | The description of the entry
    ,entryDesc   :: String
    -- | The amount of the entry 
    ,entryAmount :: Amount
  }
  deriving (Generic,Typeable,Show,Read)

type Entries = [Entry]

-- | Each account is a binding to a list of entries and a name
type Account = DML.Map Name Entries

filterEntries = DL.filter
