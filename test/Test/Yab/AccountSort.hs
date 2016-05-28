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
  prop_csvTransEntry
)
where

import Data.Budget.Entry
import Test.QuickCheck
import Test.LoremWords
import Test.Data.Budget.Budget
import Yab.AccountSort

import qualified Data.Time as DT
import qualified Data.List as DL
import qualified Data.Csv as CSV
import qualified Data.ByteString.Char8 as BS
import qualified Data.Vector as DV


-- | sample data line found in a bank's csv file.
data SampleCSVLine = SampleCSVLine 
  {
     sampleLineDay :: DT.Day 
    ,sampleLineChk :: Int 
    ,sampleLineDesc :: String 
    ,sampleAmount :: SampleCredDeb 
  }

data SampleCredDeb = SampleDeb Double | SampleCred Double

instance Arbitrary SampleCredDeb where
  arbitrary = arbitrary >>= \d -> elements $ [SampleDeb,SampleCred . negate] <*> [d]

instance Arbitrary SampleCSVLine where
  arbitrary = SampleCSVLine <$> 
    arbitrary
    <*> arbitrary
    <*> (listOf1 loremWord >>= return . DL.intercalate " ")
    <*> arbitrary

instance Show SampleCSVLine where
  show sampleLine = 
    show (sampleLineDay sampleLine)
    ++ "," ++ show (sampleLineChk sampleLine)
    ++ "," ++ show (sampleLineDesc sampleLine)
    ++ "," ++ showSampleAmnt (sampleAmount sampleLine)
    where
      showSampleAmnt (SampleDeb a) = "\"\"," ++ (show a)
      showSampleAmnt (SampleCred a) = (show a) ++ ",\"\""

instance Show SampleCredDeb where
  show (SampleDeb a) = show a
  show (SampleCred a) = show a

instance CSV.ToRecord SampleCSVLine where
  toRecord sampleLine = CSV.record [BS.pack $ show sampleLine]

prop_csvTransEntry :: SampleCSVLine -> Bool
prop_csvTransEntry sampleLine = 
  either (const False) (sampleTransEntry ==) parsedLine
  where
    sampleTransEntry = TransEntry $ Entry day desc amnt
    day = sampleLineDay sampleLine
    desc = sampleLineDesc sampleLine
    amnt = case sampleAmount sampleLine of
             SampleDeb a -> a
             SampleCred a -> a
    parsedLine = fmap DV.head $ CSV.decode CSV.NoHeader $ CSV.encode [sampleLine]
