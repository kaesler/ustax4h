module Main where

import Taxes ( bracketWidth, FilingStatus (..), OrdinaryRate (..))

main :: IO ()
main = 
  let v = bracketWidth HeadOfHousehold (OrdinaryRate 12)
  in
    print v
