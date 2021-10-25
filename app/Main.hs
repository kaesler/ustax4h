module Main where

import CommonTypes (FilingStatus (HeadOfHousehold, Single))
import Federal.OrdinaryIncome
  ( OrdinaryRate (OrdinaryRate),
    ordinaryIncomeBracketWidth,
  )
import Federal.TaxableSocialSecurity (taxableSocialSecurity)

-- Smoke test
main :: IO ()
main =
  do
    print $ ordinaryIncomeBracketWidth 2021 HeadOfHousehold (OrdinaryRate 12)
    print $ taxableSocialSecurity Single 40000.0 40000.0
