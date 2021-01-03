module Main where

import qualified Taxes as T

main :: IO ()
main = 
  let v = T.bracketWidth T.HeadOfHousehold (T.OrdinaryRate 12)
  in
    print v
