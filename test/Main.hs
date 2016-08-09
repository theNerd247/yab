{-#LANGUAGE FlexibleInstances #-}
module Main where

import Test.Tasty
import Test.Tasty.Runners
import Test.Tasty.Ingredients.Rerun
import Test.Tasty.QuickCheck

import Test.Data.Budget

main = undefined

{-main = defaultMainWithIngredients -}
  {-[-}
    {-rerunningTests defaultIngredients-}
  {-] tests-}

{-tests :: TestTree -}
{-tests = testGroup "Tests" [testGroup n ts | (n,ts) <- [-}
  {-("Serialize",serializeTests)-}
  {-,("Budget",budgetTests)-}
  {-,("CSV",csvTests)-}
  {-,("AccountSort",accountSortTests)-}
  {-]]-}
