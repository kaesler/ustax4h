module Main
  ( main,
  )
where

import AgeSpec (ageSpec)
import Federal.CalculatorSpec (agreementWithMy2017ReturnSpec, agreementWithScalaImplementationSpec)
import Federal.OrdinaryBracketsSpec (ordinaryBracketsSpec)
import Federal.QualifiedBracketsSpec (qualifiedBracketsSpec)
import Federal.RegimeSpec (futureEstimationSpec)
import Federal.TaxableSocialSecuritySpec (taxableSocialSecuritySpec)
import StateMA.CalculatorSpec (stateMATaxCalculatorSpec)
import Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
  taxableSocialSecuritySpec
  --
  ordinaryBracketsSpec
  qualifiedBracketsSpec
  agreementWithScalaImplementationSpec
  agreementWithMy2017ReturnSpec
  stateMATaxCalculatorSpec
  futureEstimationSpec
  ageSpec
