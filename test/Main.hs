module Main
  ( main,
  )
where

import Federal.CalculatorSpec (federalTaxCalculatorSpec)
import Federal.OrdinaryIncomeBracketSpec (ordinaryIncomeBracketsSpec)
import Federal.QualifiedIncomeBracketSpec (qualifiedIncomeBracketsSpec)
import Federal.TaxableSocialSecuritySpec (taxableSocialSecuritySpec)
import StateMA.CalculatorSpec (stateMATaxCalculatorSpec)
import Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
  taxableSocialSecuritySpec
  ordinaryIncomeBracketsSpec
  qualifiedIncomeBracketsSpec
  federalTaxCalculatorSpec
  stateMATaxCalculatorSpec
