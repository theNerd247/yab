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
  ,parseDate
)
where


import YabCommon
import Data.Budget

import qualified Control.Monad as CM
import qualified Data.Csv as CSV
import qualified GHC.Exts as GE (toList)

import Data.ByteString.Lazy.Char8 (unpack,pack)
import Data.Char (isSpace)

-- | A parsing exception that contains the origin parser error
data CSVParseException = CSVParseException String deriving (Show,Typeable)

instance Exception CSVParseException where
  displayException (CSVParseException s) = s

instance CSV.ToField Day where
  toField = CSV.toField . formatTime defaultTimeLocale "%x"

instance CSV.FromField Day where
  parseField f = parseDate =<< (CSV.parseField f :: CSV.Parser String)

parseDate :: (CM.MonadPlus m) => String -> m Day
parseDate s = case parseTimes s of
  [t] -> return t
  otherwise -> CM.mzero
  where
    parseTimes s = [t | (t,r) <- readSTime True defaultTimeLocale "%x" s, all isSpace r]

instance CSV.ToRecord Entry
instance CSV.FromRecord Entry

-- | Our custom CSV options 
csvEncodeOptions = CSV.defaultEncodeOptions 
  {
  -- dont quote anything
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
    mkerror s = throwM . CSVParseException $
      "Parser error in file: " ++ f ++ " \n" ++ s
