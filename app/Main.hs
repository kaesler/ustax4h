module Main where

import Taxes
  ( FilingStatus (..),
    OrdinaryRate (..),
    bracketWidth,
    taxableSocialSecurity,
  )

main :: IO ()
main =
  do
    print $ bracketWidth HeadOfHousehold (OrdinaryRate 12)
    print $ taxableSocialSecurity Single 40000.0 40000.0
