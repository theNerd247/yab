{-#LANGUAGE FlexibleInstances #-}
{-|
Module      : Name
Description : Test suite for the Data.Budget module
Copyright   : 
License     : GPL-2
Maintainer  : theNerd247
Stability   : experimental
Portability : POSIX


-}

module Test.Data.Budget.Budget
(
prop_AddAccount
,prop_RemoveAccount
,prop_MergeAccount
,prop_Newpaycheck
,prop_CheckBudgetBalanced
,prop_BudgetBalance
)
where

import Test.QuickCheck
import Test.QuickCheck.Monadic
import Data.Budget
import Test.LoremWords

import qualified Data.Monoid as DMo
import qualified Data.Map as DM
import qualified Data.Time as DT

posNum :: (Num a, Ord a, Arbitrary a) => Gen a
posNum = getPositive <$> arbitrary

instance Arbitrary Name where
  arbitrary = loremWord

instance Arbitrary DT.Day where
  arbitrary = DT.fromGregorian 
    <$> choose (2015,2018)
    <*> choose (1,12)
    <*> choose (1,31)

instance Arbitrary Entry where
  arbitrary = Entry <$> arbitrary <*> loremWord <*> arbitrary

instance Arbitrary Account where
  arbitrary = Account 
    <$> posNum
    <*> (listOf arbitrary)

instance Arbitrary (DM.Map Name Account) where
  arbitrary = do 
    DM.fromList <$> (listOf1 genItem)
    where
      genItem = ((,)) <$> loremWord <*> arbitrary

instance Arbitrary Budget where
  arbitrary = Budget 
    <$> arbitrary
    <*> suchThat posNum (/= 0)
    <*> suchThat posNum (/= 0)

newtype NonEmptyMap k a = NonEmptyMap {getNonEmptyMap :: DM.Map k a} deriving (Eq,Ord,Show)

instance (Ord k, Arbitrary k, Arbitrary a) => Arbitrary (NonEmptyMap k a) where
  arbitrary = NonEmptyMap . DM.fromList . getNonEmpty <$> arbitrary

newtype NonSingleMap k a = NonSingleMap {getNonSingleMap :: DM.Map k a} deriving (Eq,Ord,Show)

instance (Ord k, Arbitrary k, Arbitrary a) => Arbitrary (NonSingleMap k a) where
  arbitrary = NonSingleMap . DM.fromList <$> suchThat arbitrary ((1 <) . length)

prop_AddAccount :: Name -> Amount -> BudgetAccounts -> Bool
prop_AddAccount n acnt b = 
  maybe False (\_ -> True) $ DM.lookup n (addAccount n acnt b)

prop_RemoveAccount :: NonEmptyMap Name Account -> Property
prop_RemoveAccount (NonEmptyMap b) = monadicIO $ do
  n <- run . generate $ genAccName b
  assert . maybe True (\_ -> False) $ DM.lookup n =<< removeAccount n b

prop_MergeAccount :: NonSingleMap Name Account -> Property
prop_MergeAccount (NonSingleMap b) = monadicIO $ do
  n1 <- run . generate $ genAccName b
  n2 <- run . generate $ suchThat (genAccName b) (n1 /=)
  n3 <- run . generate $ suchThat arbitrary (flip DM.notMember b)
  assert . DMo.getAll . mconcat $ [
    testMerge n1 n2 True
    ,testMerge n1 n3 False
    ,testMerge n2 n3 False
    ]
  where
    expect a = (a==) . maybe False (\_ -> True)
    testMerge n1 n2 e = DMo.All . (expect e) $ DM.lookup n1 =<< mergeAccounts n1 n2 b

genAccName b = do
  i <- choose (0,(DM.size b - 1))
  return . fst $ DM.elemAt i b

prop_Newpaycheck :: Amount -> DT.Day -> Budget -> Bool
prop_Newpaycheck am d b = 
  DMo.getAll $ foldMap checkAccounts (budgetAccounts np)
  where 
    np = newPaycheck am d b
    bi = budgetIncome b
    checkAccounts a = checkEntry ((accountAmount a) * (am / bi)) . last . accountEntries $ a 
    checkEntry eam entry = DMo.All $ (testEntry eam) == entry
    testEntry a = Entry{entryDate = d, entryDesc = "new paycheck", entryAmount = a}

prop_CheckBudgetBalanced :: Property
prop_CheckBudgetBalanced = conjoin $
  [
    \b -> balanced b ==> checkBudgetBalanced b == True
    ,\b -> not (balanced b) ==> checkBudgetBalanced b == False
  ]
  where
    balanced b = ((budgetIncome b) - (sum $ accountAmount <$> (budgetAccounts b))) /= 0

-- | Is the budgetBalance function accurate?
prop_BudgetBalance :: Property
prop_BudgetBalance = conjoin $ 
  [
    \b -> emptyBudget b ==> budgetBalance b == 0
  , \b -> not (emptyBudget b) ==> (budgetBalance b) + (summedAccounts b) == (budgetIncome b)
  , \b -> not (emptyBudget b) ==> (budgetIncome b) - (budgetBalance b) == (summedAccounts b)
  ]
    where
      emptyBudget = not . DM.null . budgetAccounts
      summedAccounts b = sum (accountAmount <$> budgetAccounts b)
