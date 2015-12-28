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
)
where

import qualified Data.Csv as CSV
import qualified Data.Time as DT
import Data.ByteString.Lazy.Char8 (unpack,pack)

import Data.Budget

instance CSV.ToField DT.Day where
  toField = CSV.toField . DT.formatTime DT.defaultTimeLocale "%D"

instance CSV.FromField DT.Day where
  parseField f = (CSV.parseField f :: CSV.Parser String)
    >>= DT.parseTimeM True DT.defaultTimeLocale "%D" 

instance CSV.ToField Entry
instance CSV.FromField Entry

instance CSV.FromRecord Account where
  parseRecord = fmap (Account 0) . CSV.parseRecord

instance CSV.ToRecord Account where
  toRecord = CSV.toRecord . accountEntries

-- | A parsing exception that contains the origin parser error
data CSVParseException = CSVParseException String deriving (Show,Typeable)

instance Exception CSVParseException

-- | Our custom CSV options 
csvEncodeOptions = defaultEncodeOptions 
  {
  -- | don't quote anything....
  encQuoting = QuoteNone
  }

-- | saves a csv compatable type to a file
saveCSVFile :: (MonadIO m, ToRecord a) => FilePath -> [a] -> m ()
saveCSVFile f = liftIO . writeFile f . unpack . encodeWith csvEncodeOptions

-- | loads a headerless csv file
loadCSVFile :: (MonadThrow m, MonadIO m, FromRecord a) => FilePath -> m [a]
loadCSVFile f = do 
  bs <- liftIO $ readFile f
  either mkerror mklist . decode NoHeader . pack $ bs
    where 
    -- | convert the result from a Vector to a []
    mklist = return . toList
    -- | or convert to a SomeException
    mkerror = throwM . CSVParseException
