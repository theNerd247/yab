{-#LANGUAGE FlexibleInstances #-}
module Main where

import Test.QuickCheck
import Test.QuickCheck.Monadic 
import Test.Budget

import Data.Budget
import Data.Serialization
import System.Exit

testBudgetFilePath = "/tmp/tstBudget.yaml"

prop_yamlIO fp d = monadicIO $ do 
  run $ writeYamlFile fp d
  r <- run $ readYamlFile fp
  assert $ r == d

checkFailure :: Result -> IO a
checkFailure (Success _ _ o) = putStrLn o >> exitSuccess
checkFailure _ = exitFailure

runCheck p = checkFailure =<< verboseCheckResult p

main = do
  runCheck $ (prop_yamlIO testBudgetFilePath :: BudgetAccount -> Property)
  runCheck $ (prop_yamlIO testBudgetFilePath :: Budget -> Property)
