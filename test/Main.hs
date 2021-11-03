module Main
  ( main,
  )
where

import Federal.CalculatorSpec (agreementWithScalaImplementationSpec, agreementWithMy2017ReturnSpec)
import Federal.OrdinaryIncomeBracketSpec (ordinaryIncomeBracketsSpec)
import Federal.QualifiedIncomeBracketSpec (qualifiedIncomeBracketsSpec)
import Federal.TaxableSocialSecuritySpec (taxableSocialSecuritySpec)
import StateMA.CalculatorSpec (stateMATaxCalculatorSpec)
import Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
  taxableSocialSecuritySpec
  --
  ordinaryIncomeBracketsSpec
  qualifiedIncomeBracketsSpec
  agreementWithScalaImplementationSpec
  agreementWithMy2017ReturnSpec
  stateMATaxCalculatorSpec
