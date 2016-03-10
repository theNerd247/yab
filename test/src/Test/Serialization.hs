{-|
Module      : Name
Description : QuickCheck serialization properties for Data.Serialization
Copyright   : (c) Some Guy, 2013
License     : GPL-3
Maintainer  : sample@email.com
Stability   : experimental
Portability : POSIX

 Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod
tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At
vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren,
no sea takimata sanctus est Lorem ipsum dolor sit amet.

-}

module Test.Serialization
(
  prop_SerializeBudget
  ,prop_CSVField
  ,prop_Serialize
  ,withTempDir
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

-- | runs properties that require the temporary budget filepath and conjoins the results
withTempDir :: (Testable a) => [FilePath -> a] -> Property
withTempDir ps = monadicIO $ do 
  d <- mktempDirs
  return $ conjoin [f $ d | f <- ps]

prop_CSVField :: (Eq a, CSV.FromField a, CSV.ToField a) => a -> Bool
prop_CSVField d = 
  case CSV.runParser $ da d of
    Left _ -> False
    Right dd -> dd == d
  where
    da :: (CSV.FromField a, CSV.ToField a) => a -> CSV.Parser a
    da = CSV.parseField . CSV.toField

-- | creates a temp budget directory structure to store serialization test data
mktempDirs :: (MonadIO m) => m String
mktempDirs = liftIO $  do 
  d <- HShell.tmp_dir "/tmp/"
  HShell.mkdir $ d </> "accounts"
  return d
