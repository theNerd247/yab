{-#LANGUAGE DeriveGeneric #-}
{-#LANGUAGE FlexibleInstances #-}

{-|
Module      : Name
Description : Contains API for sorting data from navy federal CSV files to Account files
Copyright   : 
License     : GPL-2
Maintainer  : 
Stability   : experimental
Portability : POSIX

-}

module AccountSort
(
TransEntry(..)
,DescMap(..)
,EntriesMap(..)
,SortedEntries(..)
,mapTransactions
,getWordCount
,getTransEntryName
,toEntriesMap
,updateEntries
)
where

import YabCommon
import Data.Budget
import Data.Serialization

import Data.Csv ((.!))
import Control.Applicative ((<|>))
import Data.Vector ((!?))
import Data.Maybe (isJust,fromJust,isNothing)

import qualified Data.Map as DM
import qualified Data.List as DL
import qualified Data.Csv as CSV
import qualified Control.Monad as CM
import qualified Control.Applicative as CA
import qualified Data.ByteString.Char8 as BS (null)
import qualified Data.Yaml as YAML
import qualified Data.Monoid as DMO

newtype TransEntry = TransEntry {getTransEntry :: Entry} deriving (Show,Eq,Generic,Typeable)

type Keywords = [String]

type DescMap = DM.Map Name Keywords

type EntriesMap = DM.Map Name Entries

data SortedEntries = SortedEntries 
  {
    sortedEntries :: EntriesMap
  ,nonSortedEntries :: Entries
  }

data DescMapNameException = DescMapNameException FilePath [Name] deriving (Generic,Typeable)

instance Show DescMapNameException where
  show (DescMapNameException fp ns) = 
    "Accounts do not exist in transaction map file at: " ++ fp
    ++ (mconcat $ DL.intersperse "\n  " ns)

instance Exception DescMapNameException

instance CSV.FromRecord TransEntry where
  parseRecord v
    | length v == 5 = mkTransEntry <$> 
        v .! 0
        <*> v .! 2
        <*> (parseDebCred v)
    | otherwise = CM.mzero
    where
      mkTransEntry a b c = TransEntry $ Entry a b c
      parseDebCred v = maybe CM.mzero id $ do
        d <- v !? 4
        c <- v !? 3
        parseField d <|> (mkc $ parseField c)
      parseField f
          | BS.null f = CM.mzero
          | otherwise = return $ CSV.parseField f
      mkc = fmap $ fmap ((-1)*)

mapTransactions :: Entries -> DescMap -> SortedEntries
mapTransactions es dmap = SortedEntries 
  {
    sortedEntries = DM.mapKeys fromJust . DM.filterWithKey (\k _ -> isJust k) $ entriesSorted
  , nonSortedEntries = mconcat . fmap snd . DM.toList . DM.filterWithKey (\k _ -> isNothing k) $ entriesSorted
  }
  where
    entriesSorted = foldr mkmap DM.empty [(getName e dmap,[e]) | e <- es]
    getName (Entry{entryDesc = d}) dmap = getTransEntryName d dmap
    mkmap (k,a) m = DM.insertWith (++) k a m

getTransEntryName :: String -> DescMap -> Maybe Name
getTransEntryName d = fst . DM.foldrWithKey getMaxKey (Nothing,0) . fmap (getWordCount d)
  where 
    getMaxKey k v (kk,m)
      | v > m = (Just k,v)
      | otherwise = (kk,m)

getWordCount :: String -> Keywords -> Int
getWordCount d = sum . fmap getKeywordCount
  where 
    getKeywordCount keyword = length $ DL.intersect (words keyword) (words d)

toEntriesMap :: BudgetAccounts -> EntriesMap
toEntriesMap = fmap accountEntries

-- | updates the entries in the given budget accounts using the corresponding
-- entries in the given map. If no account name matches then the original budget
-- accounts is returned
updateEntries :: EntriesMap -> BudgetAccounts -> BudgetAccounts
updateEntries entryMap bas = DM.mapWithKey (\k a -> unionEntries a (DM.lookup k entryMap)) bas
  where
    unionEntries a Nothing = a
    unionEntries a (Just e) = a{accountEntries = DL.union (accountEntries a) e}
