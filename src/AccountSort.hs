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

mapTransactionsFromFile :: (MonadIO m) => FilePath -> FilePath -> m (DM.Map (Maybe Name) Entries)
mapTransactionsFromFile tFPath descMapFPath = do
  transEntries <- liftIO $ loadCSVFile tFPath
  descMap <- liftIO $ readYamlFile descMapFPath
  return $ mapTransactions (getTransEntry <$> transEntries) descMap

mapTransactions :: Entries -> DescMap -> DM.Map (Maybe Name) Entries
mapTransactions es dmap = foldr mkmap DM.empty [(getName e dmap,[e]) | e <- es]
  where
    mkmap (k,a) m = DM.insertWith (++) k a m
    getName (Entry{entryDesc = d}) dmap = getTransEntryName d dmap

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
