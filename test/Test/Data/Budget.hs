{-# LANGUAGE FlexibleInstances #-}

{-|
Module      : Name
Description : Test suite for the Data.Budget module
Copyright   : 
License     : GPL-2
Maintainer  : theNerd247
Stability   : experimental
Portability : POSIX


-}
module Test.Data.Budget ( ) where

import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Test.QuickCheck
import           Test.QuickCheck.Monadic
import           Data.Budget
import           Test.LoremWords
