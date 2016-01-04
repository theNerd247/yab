{-#LANGUAGE TypeSynonymInstances #-}
{-#LANGUAGE DeriveGeneric #-}

{-|
Module      : Name
Description : Our custom data for representing a day
Copyright   : 
License     : GPL-2
Maintainer  : theNerd247
Stability   : experimental
Portability : POSIX

-}

module Data.Budget.Day
(
  Day(..)
)
where

import YabCommon

import qualified Data.Time as DT

newtype Day = Day 
  {
  getDay :: DT.Day
  } deriving (Eq,Ord,Generic,Typeable)

instance Read Day where
  readsPrec _ s = do 
    d <- DT.parseTimeM True DT.defaultTimeLocale "%D" s 
    return (Day d,"")

instance Show Day where
  show = DT.formatTime DT.defaultTimeLocale "%D" . getDay
