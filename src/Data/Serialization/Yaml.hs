{-#LANGUAGE DeriveGeneric #-}
{-|
Module      : Name
Description : Yaml parsing
Copyright   : 
License     : GPL-2
Maintainer  : theNerd247
Stability   : experimental
Portability : POSIX

-}

module Data.Serialization.Yaml
(
  readYamlFile
  ,writeYamlFile
)
where

import YabCommon
import Data.Budget
import Data.Serialization.Errors 

import qualified Data.Yaml as YAML
import qualified Data.Aeson.Types as DAT

data BadYAMLReadException = BadYAMLReadException FilePath 
  deriving (Generic,Typeable,Read,Eq,Ord)

instance Show BadYAMLReadException where
  show (BadYAMLReadException fpath) = show $ 
    "Could not read YAML data from: " ++ fpath

instance Exception BadYAMLReadException

-- | Reads data from a yaml file. If no data could be read or an error occurs then the system exits.
readYamlFile :: (YAML.FromJSON a, MonadIO m, MonadCatch m) => FilePath -> m a
readYamlFile fpath = do 
  r <- liftIO $ YAML.decodeFile fpath 
  maybe (throwM $ BadYAMLReadException fpath) return $ r
  `catchAll` printEAndExit

-- | write data to a yaml file. If an error occurs then the system exits
writeYamlFile :: (YAML.ToJSON a, MonadIO m, MonadCatch m) => FilePath -> a -> m ()
writeYamlFile f = handleAll printEAndExit . liftIO . YAML.encodeFile f

-- | TODO: add a way to support percentages of the income in the yaml files
instance YAML.FromJSON Budget

-- TODO: fix this quick hack as per issue #290 on github.com/bos/aeson
instance YAML.ToJSON Budget

instance YAML.ToJSON Account where
  toJSON = YAML.toJSON . accountAmount

instance YAML.FromJSON Account where
  parseJSON v = do
    a <- YAML.parseJSON v
    return $ Account {accountAmount = a, accountEntries = []}
