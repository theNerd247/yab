module Data.Amount
    ( Amount
    , showAmount
    , showDecimal
    ) where

type Amount = Double

showAmount :: (Show a) => a -> String
showAmount = ("$" ++) . showDecimal

showDecimal :: (Show a) => a -> String
showDecimal d = maybe s (flip take s . (+ 3)) $ i
  where
    s = show d
    i = DL.elemIndex '.' s
