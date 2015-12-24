{-|
Module      : Name
Description : CSV file serialization
Copyright   : 
License     : GPL-2
Maintainer  : theNerd247
Stability   : experimental
Portability : POSIX

-}

module Data.Serialization.Csv
(
)
where

import qualified Data.Csv as CSV
import qualified Data.Time as DT

import Data.Budget.Entry

instance CSV.ToField DT.Day where
  toField = CSV.toField . DT.formatTime DT.defaultTimeLocale "%D"

instance CSV.FromField DT.Day where
  parseField f = (CSV.parseField f :: CSV.Parser String)
    >>= DT.parseTimeM True DT.defaultTimeLocale "%D" 

instance CSV.ToField Entry
instance CSV.FromField Entry
