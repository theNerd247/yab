{-|
Module      : YabCommon
Description : Common imports and types used for yab
Copyright   : 
License     : GPL-2
Maintainer  : theNerd247
Stability   : experimental
Portability : POSIX

-}

module YabCommon
(
   module Control.Monad.Catch
  ,module Control.Monad.IO.Class
  ,module Data.Scientific
  ,module Data.Typeable
  ,module GHC.Generics
  ,Name
  ,Rate
  ,Amount
)
where

import Control.Monad.Catch 
import Control.Monad.IO.Class
import Data.Scientific
import Data.Typeable
import GHC.Generics

type Name = String

type Rate = Integer

type Amount = Double
