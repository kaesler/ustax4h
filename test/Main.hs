module Main
  ( main,
  )
where

import Federal.CalculatorSpec (agreementWithMy2017ReturnSpec, agreementWithScalaImplementationSpec)
import Federal.OrdinaryIncomeBracketSpec (ordinaryIncomeBracketsSpec)
import Federal.QualifiedIncomeBracketSpec (qualifiedIncomeBracketsSpec)
import Federal.RegimeSpec (futureEstimationSpec)
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
  futureEstimationSpec
