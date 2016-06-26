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

module Test.Data.Budget
(
budgetTests
)
where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Data.Budget
import Test.LoremWords

import qualified Data.Monoid as DMo
import qualified Data.Map as DM
import qualified Data.Time as DT
import qualified Data.List as DL

newtype BalancedBudget = BalancedBudget {getBalancedBudget :: Budget} deriving Show

newtype UnbalancedBudget = UnbalancedBudget {getUnbalancedBudget :: Budget} deriving Show

newtype NonEmptyMap k a = NonEmptyMap {getNonEmptyMap :: DM.Map k a} deriving (Eq,Ord,Show)

newtype NonSingleMap k a = NonSingleMap {getNonSingleMap :: DM.Map k a} deriving (Eq,Ord,Show)

newtype EmptyBudget = EmptyBudget {getEmptyBudget :: Budget} deriving Show

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
    DM.fromList <$> (listOf genItem)
    where
      genItem = ((,)) <$> loremWord <*> arbitrary

instance Arbitrary Budget where
  arbitrary = Budget 
    <$> arbitrary
    <*> suchThat posNum (/= 0)
    <*> suchThat posNum (/= 0)

instance Arbitrary BalancedBudget where
  arbitrary = BalancedBudget <$> do
    accounts <- arbitrary
    rate <- suchThat posNum (/=0)
    return $ Budget 
      {
        budgetAccounts = accounts 
      , budgetIncome = (sum $ accountAmount <$> accounts) 
      , budgetRate = rate
      }

instance Arbitrary UnbalancedBudget where
  arbitrary = UnbalancedBudget <$> do
    -- prevent an empty budget list because this will cause
    -- checkBudgetBalanced to succeed.
    accounts <- suchThat arbitrary (not . DM.null)
    income <- suchThat posNum (/= (sum $ accountAmount <$> accounts))
    rate <- suchThat posNum (/= 0)
    return $ Budget 
      {
        budgetAccounts = accounts
      , budgetIncome = income
      , budgetRate = rate
      }
    
instance (Ord k, Arbitrary k, Arbitrary a) => Arbitrary (NonEmptyMap k a) where
  arbitrary = NonEmptyMap . DM.fromList . getNonEmpty <$> arbitrary

instance (Ord k, Arbitrary k, Arbitrary a) => Arbitrary (NonSingleMap k a) where
  arbitrary = NonSingleMap . DM.fromList <$> suchThat arbitrary ((1 <) . length)

instance Arbitrary EmptyBudget where
  arbitrary = EmptyBudget <$> do
    budget <- arbitrary
    return $ budget {budgetAccounts = DM.empty}

budgetTests :: [TestTree]
budgetTests = [
  testProperty "add_account" prop_AddAccount
  ,testProperty "remove_account" prop_RemoveAccount
  ,testProperty "merge_account" prop_MergeAccount
  ,testProperty "new_paycheck" prop_Newpaycheck
  ,testProperty "check_budget_balanced" prop_CheckBudgetBalanced
  ,testProperty "check_budget_unbalanced" prop_CheckBudgetUnBalanced
  ,testProperty "budget_balance_empty" prop_BudgetBalanceEmpty
  ,testProperty "budget_balance_nonempty" prop_BudgetBalanceNonEmpty
  ,testProperty "account_balance_nonempty" prop_AccountBalanceNonEmpty
  ]

posNum :: (Num a, Ord a, Arbitrary a) => Gen a
posNum = getPositive <$> arbitrary

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

prop_CheckBudgetBalanced :: BalancedBudget -> Bool
prop_CheckBudgetBalanced b = checkBudgetBalanced (getBalancedBudget b) == True

prop_CheckBudgetUnBalanced :: UnbalancedBudget -> Bool
prop_CheckBudgetUnBalanced b = checkBudgetBalanced (getUnbalancedBudget b) == False

-- | Is the budgetBalance function accurate?
prop_BudgetBalanceEmpty :: EmptyBudget -> Bool
prop_BudgetBalanceEmpty = (==0) . budgetBalance . getEmptyBudget

prop_BudgetBalanceNonEmpty :: Budget -> Property
prop_BudgetBalanceNonEmpty b = 
  nonEmptyBudget b ==> 
    counterexample failMsg
    -- Run our equality test with 11 decimal precision. Anything beyond that may
    -- not be necessary?
    $ floatEq (bal + summedAccounts) (budgetIncome b) amountPrecision
  where
    bal = budgetBalance b
    nonEmptyBudget = not . DM.null . budgetAccounts
    summedAccounts = sum $ accountAmount <$> (budgetAccounts b)
    failMsg = 
      "Balance: " ++ (show bal) 
      ++ "\n" ++ "SummedAccounts: " ++ (show summedAccounts)
      ++ "\n" ++ "Diff: " ++ (show $ bal + summedAccounts - budgetIncome b)

-- | Test equality of floating point numbers given an error. For example:
-- > floatEq 1.23 1.234 1e-3 
-- will show the numbers are equivalent as long as the absolute difference is
-- less than or equal to 1e-3.
floatEq :: RealFloat a => a -> a -> a -> Bool 
floatEq a b c = abs(a - b) <= abs(c)

amountPrecision = 1e-9

prop_AccountBalanceNonEmpty :: Account -> Property
prop_AccountBalanceNonEmpty a = 
  not (DL.null $ accountEntries a) ==>
  counterexample failMsg
  $ floatEq bal expected amountPrecision
  where
    expected = sum . fmap entryAmount . accountEntries $ a
    bal = accountBalance a
    failMsg = 
      "Balance: " ++ (show bal)
      ++ "\n" ++ "Expected: " ++ (show expected)
      ++ "\n" ++ "Diff: " ++ (show $ expected - bal)
