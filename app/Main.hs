module Main where

import CommonTypes (FilingStatus (HeadOfHousehold, Single))
import qualified Federal.TaxableSocialSecurity as TSS

-- Smoke test
main :: IO ()
main =
  do
    print $ TSS.amountTaxable Single 40000.0 40000.0
