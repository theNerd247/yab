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
  ,dayFormats
  ,csvEncodeOptions
)
where

import YabCommon
import Data.Budget

import qualified Control.Monad as CM
import qualified Control.Applicative as CA
import qualified Data.Csv as CSV
import qualified GHC.Exts as GE (toList)
import qualified Data.ByteString.Char8 as BS (null)
import Control.Applicative ((<|>))

import Data.ByteString.Lazy.Char8 (unpack,pack)
import Data.Char (isSpace)
import Data.Csv ((.!))
import Data.Vector ((!?))

-- | A parsing exception that contains the origin parser error
data CSVParseException = CSVParseException String deriving (Show,Typeable)

instance Exception CSVParseException where
  displayException (CSVParseException s) = s

instance CSV.ToField Day where
  toField = CSV.toField . formatTime defaultTimeLocale "%x"

instance CSV.FromField Day where
  parseField f = parseDate =<< (CSV.parseField f :: CSV.Parser String)

instance CSV.ToRecord Entry
instance CSV.FromRecord Entry

-- | Our custom CSV options 
csvEncodeOptions = CSV.defaultEncodeOptions 
  {
  -- dont quote anything
  CSV.encQuoting = CSV.QuoteNone
  }

dayFormats = [
  "%"++u++"m"
  ++s++"%"++u++"d"
  ++s++"%"++y
  -- note: y must be parsed before Y
  | u <- ["","_","0","-"], y <- ["y","Y"], s <- ["/","-"]
  ]


parseDate :: (CM.MonadPlus m) => String -> m Day
parseDate s = case parseTimes s of
  [] -> CM.mzero
  (x:xs) -> return x
  where
    parseTimes s = mconcat $ parseFormat <$> dayFormats <*> pure s
    parseFormat f s = [t | (t,r) <- readSTime True defaultTimeLocale f s, all isSpace r]

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
    mkerror s = throwM . CSVParseException $ s
      {-"Parser error in file: " ++ f ++ "\n" ++ " on line containing: " ++ s -- (lines s !! 1)-}
