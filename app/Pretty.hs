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

import           YabCommon
import           Data.Budget
import           Yab.AccountSort

import qualified Text.PrettyPrint.HughesPJClass as TPH
import qualified Data.Map                       as DM

import           Text.PrettyPrint.HughesPJClass ( ($+$), (<+>), (<>) )

instance TPH.Pretty Entry where
    pPrint e = (TPH.pPrint $ entryDate e)
        <+> (TPH.double $ entryAmount e)
            <+> (TPH.text $ entryDesc e)
    pPrintList _ = TPH.vcat . fmap TPH.pPrint

instance TPH.Pretty Day where
    pPrint = TPH.text . show

instance TPH.Pretty SortedEntries where
    pPrint s = printPrettyMap (sortedEntries s)
        $+$ (printPrettyMap . DM.singleton "Other" $ nonSortedEntries s)

instance TPH.Pretty EntriesMap where
    pPrint = printPrettyMap

printPrettyMap :: (TPH.Pretty k, TPH.Pretty a) => DM.Map k a -> TPH.Doc
printPrettyMap = DM.foldrWithKey (\k a -> ($+$ printPrettyMapElem k a))
                                 TPH.empty
  where
    printPrettyMapElem k a =
        TPH.pPrint k <> TPH.colon
            $+$ TPH.nest 2 (TPH.pPrint a)