{-|
Module      : Name
Description : A QuickCheck generator for legible words using a lorem ipsum paragraph
Copyright   : 
License     : GPL-2
Maintainer  : theNerd247
Stability   : experimental
Portability : POSIX

-}

module Test.LoremWords
(
  loremWord
)
where

import Test.QuickCheck

loremParagraph = "Lorem ipsum dolor sit amet consetetur sadipscing elitr sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat sed diam voluptua At vero eos et accusam et justo duo dolores et ea rebum Stet clita kasd gubergren no sea takimata sanctus est Lorem ipsum dolor sit amet" 

loremWord = elements . words $ loremParagraph
