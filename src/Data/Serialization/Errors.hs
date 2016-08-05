{-|
Module      : Name
Description : Stores different handlers for errors that could occur during serialization.
Copyright   : 
License     : GPL-2
Maintainer  : theNerd247
Stability   : experimental
Portability : POSIX

More specific error handling (such as for YAML serialization) should be handled
within its respective module.
-}
module Data.Serialization.Errors ( printEAndExit ) where

import           YabCommon

import qualified System.Exit       as SE
import qualified Control.Exception as CE

printEAndExit :: (MonadIO m, CE.Exception e) => e -> m a
printEAndExit e = do
    liftIO . putStrLn . displayException $ e
    liftIO SE.exitFailure