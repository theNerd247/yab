{-|
Module      : Name
Description : 
Copyright   :
License     : GPL-2
Maintainer  : sample@email.com
Stability   : experimental
Portability : POSIX

-}

module Test.Data.Serialization
(
  prop_SerializeBudget
  ,prop_Serialize
  ,mktempDirs
)
where

import YabCommon

import Data.Serialization
import Data.Budget
import Test.QuickCheck
import Test.QuickCheck.Monadic
import System.FilePath.Posix ((</>))

import qualified Data.Csv as CSV
import qualified HsShellScript as HShell

prop_Serialize :: (Serialize a, Eq a) => FilePath -> a -> Property
prop_Serialize fp d = monadicIO $ do 
  run $ serialize fp d
  r <- run $ deserialize fp
  assert $ r == d

prop_SerializeBudget :: FilePath -> Budget -> Property
prop_SerializeBudget = prop_Serialize . (</> "budget.yaml")

-- | creates a temp budget directory structure to store serialization test data
mktempDirs :: (MonadIO m) => m String
mktempDirs = liftIO $  do 
  d <- HShell.tmp_dir "/tmp/"
  HShell.mkdir $ d </> "accounts"
  return d
