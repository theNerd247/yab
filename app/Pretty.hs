{-|
Module      : Name
Description : 
Copyright   : (c) Some Guy, 2013
License     : GPL-3
Maintainer  : sample@email.com
Stability   : experimental
Portability : POSIX


-}

module Pretty
(
)
where

import YabCommon
import Data.Budget
import AccountSort
import Text.PrettyPrint.HughesPJClass

instance Pretty Entry where
  pPrint e = (pPrint $ entryDate e) 
        <+> (float $ entryAmount e)
        <+> (text $ entryDesc e)

instance Pretty Date where
  pPrint = text . show
