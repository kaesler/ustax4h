module Main
  ( main,
  )
where

import AgeSpec (ageSpec)
import Federal.CalculatorSpec (agreementWithScalaImplementationForFutureYearsSpec, agreementWithScalaImplementationForKnownYearsSpec)
import Federal.OrdinaryBracketsSpec (ordinaryBracketsSpec)
import Federal.QualifiedBracketsSpec (qualifiedBracketsSpec)
import Federal.RegimeSpec (futureEstimationSpec)
import Federal.TaxableSocialSecuritySpec (taxableSocialSecuritySpec)
import Federal.Yearly.YearlyValuesSpec (averageThresholdChangeSpec)
import StateMA.CalculatorSpec (stateMATaxCalculatorSpec)
import Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
  taxableSocialSecuritySpec
  --
  ordinaryBracketsSpec
  qualifiedBracketsSpec
  agreementWithScalaImplementationForFutureYearsSpec
  agreementWithScalaImplementationForKnownYearsSpec
  stateMATaxCalculatorSpec
  futureEstimationSpec
  ageSpec
  averageThresholdChangeSpec

