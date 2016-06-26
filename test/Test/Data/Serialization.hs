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
serializeTests
)
where

import YabCommon

import Test.Tasty
import Test.Tasty.QuickCheck
import Data.Serialization
import Data.Budget
import Test.QuickCheck
import Test.QuickCheck.Monadic
import System.FilePath.Posix ((</>))

import qualified Data.Csv as CSV
import qualified HsShellScript as HShell

import Test.Data.Budget

serializeTests :: [TestTree]
serializeTests = [runSerializeTest $ \dir -> testGroup "Budget Dir Tests" 
  [
    testProperty "budget_serialization" (prop_SerializeBudget dir)
  ]]

runSerializeTest :: (IO FilePath -> TestTree) -> TestTree
runSerializeTest tsts = withResource (mktempDirs) (\_ -> return ()) tsts

prop_Serialize :: (Serialize a, Eq a) => IO FilePath -> a -> Property
prop_Serialize filePath d = monadicIO $ do 
  fp <- run $ filePath
  run $ serialize fp d
  r <- run $ deserialize fp
  assert $ r == d

prop_SerializeBudget :: IO FilePath -> Budget -> Property
prop_SerializeBudget dir = prop_Serialize $ do 
  d <- dir
  return $ d </> "budget.yaml"

-- | creates a temp budget directory structure to store serialization test data
mktempDirs :: (MonadIO m) => m FilePath
mktempDirs = liftIO $  do 
  d <- HShell.tmp_dir "/tmp/"
  HShell.mkdir $ d </> "accounts"
  return d
