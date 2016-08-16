{-#LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Name
Description : Short description
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
module Test.Yab.MongoDB ( mongoDBTests ) where

import           Test.QuickCheck
import           Test.QuickCheck.Monadic
import           Test.Tasty.QuickCheck
import           Test.Tasty
import           Data.Budget
import           Control.Lens
import           Data.Tree.Lens
import           Database.MongoDB
import           Yab.MongoDB
import           Test.Data.Budget

testDB = "testDB" :: Database

clearDB :: IO ()
clearDB = runMongoCmd testDB (delete $ select [] "budgets")

pushPullBudget_prop :: Budget -> Property
pushPullBudget_prop b = monadicIO $ do
    run $ pushBudget testDB b
    testB <- run $ pullBudget testDB $ b ^. root . budgetName
    assert $ maybe False (== b) testB

deleteBudget_prop :: Budget -> Property
deleteBudget_prop b = monadicIO $ do
    run $ pushBudget testDB b
    run $ deleteBudget testDB bName
    b <- run $ pullBudget testDB bName
    assert (b == Nothing)
  where
    bName = b ^. root . budgetName

mkDBTestProp :: (Testable a) => TestName -> a -> TestTree
mkDBTestProp n a = withResource clearDB (const clearDB) $ const (testProperty n a)

mongoDBTests = [ (mkDBTestProp "pushPullBudget" pushPullBudget_prop)
               , (mkDBTestProp "deleteBudget" deleteBudget_prop)
               ]
