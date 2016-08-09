{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}

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
    ( TPH.prettyShow
    , printPrettyMap
    ) where

import           Data.Budget
import           Control.Lens
import qualified Text.PrettyPrint.HughesPJClass as TPH
import qualified Data.Map                       as DM
import           Data.Time
import           Text.PrettyPrint.HughesPJClass ( ($+$), (<+>), (<>) )

instance TPH.Pretty Entry where
    pPrint e = (TPH.pPrint $ e ^. entryDate)
        <+> (TPH.double $ e ^. entryAmount)
        <+> (TPH.text $ e ^. entryDesc)

    pPrintList _ = TPH.vcat . fmap TPH.pPrint

instance TPH.Pretty Day where
    pPrint = TPH.text . show

printPrettyMap :: (TPH.Pretty k, TPH.Pretty a) => DM.Map k a -> TPH.Doc
printPrettyMap = DM.foldrWithKey (\k a -> ($+$ printPrettyMapElem k a))
                                 TPH.empty
  where
    printPrettyMapElem k a =
        TPH.pPrint k <> TPH.colon
            $+$ TPH.nest 2 (TPH.pPrint a)
