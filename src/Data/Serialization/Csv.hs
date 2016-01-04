{-|
Module      : Name
Description : CSV file serialization
Copyright   : 
License     : GPL-2
Maintainer  : theNerd247
Stability   : experimental
Portability : POSIX

-}

module Data.Serialization.Csv
(
  saveCSVFile
  ,loadCSVFile
  ,CSVParseException(..)
)
where


import YabCommon
import Data.Budget

import qualified Data.Csv as CSV

import qualified GHC.Exts as GE (toList)
import Data.ByteString.Lazy.Char8 (unpack,pack)

instance CSV.ToField Day where
  toField = CSV.toField . show . getDay

instance CSV.FromField Day where
  parseField f = (CSV.parseField f :: CSV.Parser String)
    >>= return . Day . read 

instance CSV.ToRecord Entry
instance CSV.FromRecord Entry

-- | A parsing exception that contains the origin parser error
data CSVParseException = CSVParseException String deriving (Show,Typeable)

instance Exception CSVParseException

-- | Our custom CSV options 
csvEncodeOptions = CSV.defaultEncodeOptions 
  {
  -- | don't quote anything....
  CSV.encQuoting = CSV.QuoteNone
  }

-- | saves a csv compatable type to a file
saveCSVFile :: (MonadIO m, CSV.ToRecord a) => FilePath -> [a] -> m ()
saveCSVFile f = liftIO . writeFile f . unpack . CSV.encodeWith csvEncodeOptions

-- | loads a headerless csv file
loadCSVFile :: (MonadThrow m, MonadIO m, CSV.FromRecord a) => FilePath -> m [a]
loadCSVFile f = do 
  bs <- liftIO $ readFile f
  either mkerror mklist . CSV.decode CSV.NoHeader . pack $ bs
    where 
    -- | convert the result from a Vector to a []
    mklist = return . GE.toList
    -- | or convert to a SomeException
    mkerror = throwM . CSVParseException
