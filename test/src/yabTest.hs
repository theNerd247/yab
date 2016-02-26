{-#LANGUAGE FlexibleInstances #-}
module Main where

import Test.QuickCheck
import Test.QuickCheck.Monadic 
import Test.Budget

import Data.Budget
import Data.Serialization
import System.Exit

import qualified Data.Csv as CSV
import qualified HsShellScript as HShell

import System.FilePath.Posix ((</>))
import Control.Monad (mzero)

import YabCommon

prop_Serialize fp d = monadicIO $ do 
  run $ serialize fp d
  r <- run $ deserialize fp
  assert $ r == d

prop_CSVField :: (Eq a, CSV.FromField a, CSV.ToField a) => a -> Bool
prop_CSVField d = 
  case CSV.runParser $ da d of
    Left _ -> False
    Right dd -> dd == d
  where
    da :: (CSV.FromField a, CSV.ToField a) => a -> CSV.Parser a
    da = CSV.parseField . CSV.toField

mktempDir :: (MonadIO m) => m String
mktempDir = liftIO $ HShell.tmp_dir "/tmp/"

checkFailure :: Result -> IO a
checkFailure (Success _ ls _) = do 
  putStrLn $ "Labels: " ++ (show ls)
  exitSuccess

checkFailure Failure{labels = ls} = do
  putStrLn $ "Labels: " ++ (show ls)
  exitFailure

checkMult :: Double -> Double -> Double -> Bool
checkMult a b c = (a * b / c) == (a * (b/c))

runCheck p = checkFailure =<< quickCheckResult p

main = do
  tstDir <- mktempDir
  HShell.mkdir $ tstDir </> "accounts"
  putStrLn $ "Test data at: " ++ tstDir
  runCheck $ (prop_CSVField :: Day -> Bool)
  runCheck $ (prop_Serialize (tstDir </> "budget.yaml") :: Budget -> Property)
  runCheck $ prop_AddAccount
  runCheck $ prop_RemoveAccount
  runCheck $ prop_MergeAccount
  runCheck $ prop_Newpaycheck
  runCheck $ checkMult
