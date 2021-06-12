module Main where

import Taxes
  ( FilingStatus (..),
    OrdinaryRate (..),
    ordinaryIncomeBracketWidth,
    taxableSocialSecurity,
  )

main :: IO ()
main =
  do
    print $ ordinaryIncomeBracketWidth HeadOfHousehold (OrdinaryRate 12)
    print $ taxableSocialSecurity Single 40000.0 40000.0
