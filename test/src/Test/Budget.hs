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

module Test.Budget
(
prop_AddAccount
,prop_RemoveAccount
,prop_MergeAccount
,prop_Newpaycheck
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

prop_AddAccount :: Name -> Amount -> BudgetAccounts -> Bool
prop_AddAccount n acnt b = 
  maybe False (\_ -> True) $ DM.lookup n (addAccount n acnt b)

prop_RemoveAccount :: NonEmptyMap Name Account -> Property
prop_RemoveAccount (NonEmptyMap b) = monadicIO $ do
  n <- run . generate $ genAccName b
  assert . maybe True (\_ -> False) $ DM.lookup n =<< removeAccount n b

prop_MergeAccount :: NonEmptyMap Name Account -> Property
prop_MergeAccount (NonEmptyMap b) = monadicIO $ do
  n1 <- run . generate $ genAccName b
  n2 <- run . generate $ suchThat (genAccName b) (n1 /=)
  assert . maybe False (\_ -> True) $ DM.lookup n1 =<< mergeAccounts n1 n2 b

genAccName b = do
  i <- choose (0,(DM.size b - 1))
  return . fst $ DM.elemAt i b

prop_Newpaycheck :: Amount -> DT.Day -> Budget -> Property
prop_Newpaycheck am d b = 
  label ("Newpaycheck: " ++ (show np))
  $ DMo.getAll $ foldMap checkAccounts (budgetAccounts np)
  where 
    np = newPaycheck am d b
    bi = budgetIncome b
    checkAccounts a = checkEntry ((accountAmount a) * (am / bi)) . last . accountEntries $ a 
    checkEntry eam entry = DMo.All $ (testEntry eam) == entry
    testEntry a = Entry{entryDate = d, entryDesc = "new paycheck", entryAmount = a}
