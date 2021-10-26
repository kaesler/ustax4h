module Main where

import CommonTypes (FilingStatus (HeadOfHousehold, Single))
import Federal.TaxableSocialSecurity (taxableSocialSecurity)

-- Smoke test
main :: IO ()
main =
  do
    print $ taxableSocialSecurity Single 40000.0 40000.0
