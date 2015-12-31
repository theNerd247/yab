{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE TupleSections #-}
{-|
Module      : Name
Description : Serialization modules
Copyright   : 
License     : GPL-4
Maintainer  : theNerd247
Stability   : experimental
Portability : POSIX

-}

module Data.Serialization
(
  module Data.Serialization.Yaml
  ,module Data.Serialization.Csv
  ,Serialize(..)
)
where

import YabCommon
import Data.Budget
import Data.Serialization.Yaml
import Data.Serialization.Csv

import qualified Data.Map.Lazy as DM
import qualified System.FilePath.Posix as SFP
import qualified System.Directory as SD

import System.FilePath.Posix ((</>),(<.>))

-- | Class for creating a serialization interface (mainly to unify the CSV and YAML interfaces).
-- | The default implementation for this type is to print it to the file using @show@
class (Show a, Read a) => Serialize a where
  -- | function to serialize a type out. Any errors should be handled through
  -- exceptions
  serialize :: (MonadIO m, MonadCatch m) => FilePath -> a -> m ()
  serialize fpath = liftIO . writeFile fpath . show

  -- | function to deserialize a type in from a given file. Any errors should be
  -- handled through exceptions
  deserialize :: (MonadIO m, MonadCatch m) => FilePath -> m a
  deserialize fpath = liftIO $ read <$> readFile fpath

instance Serialize Account where
  serialize fpath = saveCSVFile fpath . accountEntries
  deserialize fpath = Account 0 <$> loadCSVFile fpath

instance Serialize (DM.Map Name Account) where
  serialize fpath m = sequence_ 
    [serialize (makeAccountPath fpath n) a | (n,a) <- DM.toList m]

  deserialize fpath = do
    af <- accountFiles
    as <- sequence (deserialize <$> af) 
    return $ DM.fromList (zip af as)
    where
      accountFiles = (liftIO . SD.getDirectoryContents $ fpath </> "accounts")
                     >>= return . filter ((==".csv") . SFP.takeExtension)

instance Serialize Budget where
  serialize fpath b = do
    -- save the budget yaml file
    writeYamlFile (fpath </> "budget.yaml") b
    -- save the entries for each account
    serialize fpath (budgetAccounts b)

  deserialize fpath = do
    -- serialize in from the budget config file
    b <- readYamlFile (fpath </> "budget.yaml")
    -- serialize in the entry files (only those mentioned in the budget file)
    cas <- sequence $ deserializeAccount fpath <$> DM.keys (budgetAccounts b)
    -- merge the Account data
    return $ b 
      {
        budgetAccounts = DM.unionWith mergeAccounts (DM.fromList cas) (budgetAccounts b)
      }

makeAccountPath basePath name = basePath </> "accounts" </> name <.> "csv"
deserializeAccount bp n = deserialize (makeAccountPath bp n) >>= return . ((n,))
