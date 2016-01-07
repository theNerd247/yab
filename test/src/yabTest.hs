{-#LANGUAGE FlexibleInstances #-}
module Main where

import Test.QuickCheck
import Test.QuickCheck.Monadic 
import Test.Budget

import Data.Budget
import Data.Serialization
import System.Exit

import qualified HsShellScript as HShell
import System.FilePath.Posix ((</>))


import YabCommon

prop_Serialize fp d = monadicIO $ do 
  run $ serialize fp d
  r <- run $ deserialize fp
  assert $ r == d

mktempDir = liftIO $ HShell.tmp_dir "/tmp/"

checkFailure :: Result -> IO a
checkFailure (Success _ _ o) = putStrLn o >> exitSuccess
checkFailure _ = exitFailure

runCheck p = checkFailure =<< verboseCheckResult p

main = do
  tstDir <- mktempDir
  HShell.mkdir $ tstDir </> "accounts"
  putStrLn $ "Test data at: " ++ tstDir
  runCheck $ (prop_Serialize (tstDir </> "budget.yaml") :: Budget -> Property)
