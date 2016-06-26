{-|
Module      : Name
Description : Test suite for Yab.AccountSort
Copyright   : 
License     : GPL-2
Maintainer  : 
Stability   : experimental
Portability : POSIX


-}

module Test.Yab.AccountSort
(
  accountSortTests
)
where

import Test.Tasty
import Test.Tasty.QuickCheck

import Data.Budget.Entry
import Data.Serialization.Csv (csvEncodeOptions)
import Test.QuickCheck
import Test.LoremWords
import Test.Data.Budget.Budget
import Yab.AccountSort

import qualified Data.Time as DT
import qualified Data.List as DL
import qualified Data.Csv as CSV
import qualified Data.ByteString.Char8 as BS
import qualified Data.Vector as DV

accountSortTests :: [TestTree]
accountSortTests = [
  testProperty "csv_TransEntry" prop_csvTransEntry
  ]

-- | sample data line found in a bank's csv file.
data SampleCSVLine = SampleCSVLine 
  {
     sampleLineDay :: DT.Day 
    ,sampleLineChk :: Int 
    ,sampleLineDesc :: String 
    ,sampleAmount :: Double 
  }

instance Arbitrary SampleCSVLine where
  arbitrary = SampleCSVLine <$> 
    arbitrary
    <*> arbitrary
    <*> (listOf1 loremWord >>= return . DL.intercalate " ")
    <*> arbitrary

instance Show SampleCSVLine where
  show sampleLine = 
    DT.formatTime DT.defaultTimeLocale "%m/%d/%Y" (sampleLineDay sampleLine)
    ++ "," ++ show (sampleLineChk sampleLine)
    ++ "," ++ show (sampleLineDesc sampleLine)
    ++ "," ++ showSampleAmnt (sampleAmount sampleLine)
    where
      showSampleAmnt a
        | a < 0 = "\"\"," ++ (show . negate $ a)
        | otherwise = (show a) ++ ",\"\""

instance CSV.ToRecord SampleCSVLine where
  toRecord sampleLine = CSV.record [BS.pack $ show sampleLine]

prop_csvTransEntry :: SampleCSVLine -> Property
prop_csvTransEntry sampleLine = 
    counterexample failMsg
  $ either (const False) (sampleTransEntry ==) parsedLine
  where
    sampleTransEntry = TransEntry $ Entry day desc amnt
    day = sampleLineDay sampleLine
    desc = sampleLineDesc sampleLine
    amnt = sampleAmount sampleLine
    parsedLine = fmap DV.head $ CSV.decode CSV.NoHeader encodedLine
    encodedLine = CSV.encodeWith csvEncodeOptions [sampleLine]
    failMsg =
        ("sampleLine: " ++ (show sampleLine)) 
      ++ "\n" ++ ("sampleTransEntry: " ++ (show sampleTransEntry))
      ++ "\n" ++ ("parsedLine: " ++ (show parsedLine))
