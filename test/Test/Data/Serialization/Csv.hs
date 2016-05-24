{-|
Module      : Name
Description : Test suite for Data.Serialization.Csv
Copyright   : 
License     : GPL-2
Maintainer  : sample@email.com
Stability   : experimental
Portability : POSIX

-}

module Test.Data.Serialization.Csv
(
  prop_CSVField
, prop_invalidDate
, prop_validDate
)
where

import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.Data.Budget.Budget

import Data.Serialization.Csv

import qualified Data.Time as DT
import qualified Data.Csv as CSV

newtype DayString = DayString {getDayString :: String} 
  deriving (Eq,Ord,Show,Read)

instance Arbitrary DayString where
  arbitrary = do 
    day <- arbitrary :: Gen DT.Day
    format <- elements dayFormats
    return . DayString $ DT.formatTime DT.defaultTimeLocale format day

-- | Test conversion of data type to CSV field
prop_CSVField :: (Eq a, CSV.FromField a, CSV.ToField a) => a -> Bool
prop_CSVField d = 
  case CSV.runParser $ da d of
    Left _ -> False
    Right dd -> dd == d
  where
    da :: (CSV.FromField a, CSV.ToField a) => a -> CSV.Parser a
    da = CSV.parseField . CSV.toField

prop_invalidDate :: String -> Property
prop_invalidDate s = collect s $ 
  forAll (getDayString <$> (arbitrary :: Gen DayString)) $ \ds ->
      (ds /= s) ==> (maybeToBool False $ parseDate s)

prop_validDate :: DayString -> Bool
prop_validDate = maybeToBool True . parseDate . getDayString

maybeToBool b = maybe (not b) (const b) 
