{-|
Module      : YabCommon
Description : Common imports, types, and functions used for yab
Copyright   : 
License     : GPL-2
Maintainer  : theNerd247
Stability   : experimental
Portability : POSIX

-}
module YabCommon
    ( module Control.Monad.Catch
    , module Control.Monad.IO.Class
    , module Data.Scientific
    , module Data.Typeable
    , module GHC.Generics
    , module Data.Time
    , today
    ) where

import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Data.Scientific
import           Data.Typeable
import           GHC.Generics
import           Data.Time

today :: (MonadIO m) => m Day
today = liftIO $ getCurrentTime >>= return . utctDay