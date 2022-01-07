module Main where

import CommonTypes (FilingStatus (HeadOfHousehold, Single))
import qualified Federal.TaxableSocialSecurity as TSS
import Moneys (makeFromInt)

-- Smoke test
main :: IO ()
main =
  do
    print $ TSS.amountTaxable Single (makeFromInt 40000) (makeFromInt 40000)
