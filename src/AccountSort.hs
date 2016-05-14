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
,SortedEntries(..)
,mapTransactionsFromFile
,mapTransactions
,getWordCount
,getTransEntryName
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

newtype TransEntry = TransEntry {getTransEntry :: Entry} deriving (Show,Eq,Generic,Typeable)

type Keywords = [String]

type DescMap = DM.Map Name Keywords

data SortedEntries = SortedEntries 
  {
    sortedEntries :: DM.Map Name Entries
  ,nonSortedEntries :: Entries
  }

instance TermPrint SortedEntries where
  printTerm s = 
    printTerm (sortedEntries s) 
    ++ "\n " 
    ++ (printTerm DM.singleton "Other" $ nonSortedEntries s)

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

mapTransactionsFromFile :: (MonadIO m) => FilePath -> FilePath -> m SortedEntries
mapTransactionsFromFile tFPath descMapFPath = do
  transEntries <- liftIO $ loadCSVFile tFPath
  descMap <- liftIO $ readYamlFile descMapFPath
  return $ mapTransactions (getTransEntry <$> transEntries) descMap

mapTransactions :: Entries -> DescMap -> SortedEntries
mapTransactions es dmap = SortedEntries 
  {
    sortedEntries = DM.mapKeys fromJust . DM.filter isJust $ entriesSorted
    nonSortedEntries = fmap snd . DM.toList . DM.filter isNothing $ entriesSorted
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
