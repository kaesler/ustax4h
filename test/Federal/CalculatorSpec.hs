{-# LANGUAGE RecordWildCards #-}

module Federal.CalculatorSpec
  ( agreementWithScalaImplementationForFutureYearsSpec,
    agreementWithScalaImplementationForKnownYearsSpec,
  )
where

import Federal.Calculator (FederalTaxResults (..), makeCalculator, taxDueForKnownYear, taxDueForFutureYear, taxDueForKnownYearDebug)
import Federal.Regime (Regime (..))
import Federal.Types (OrdinaryIncome, QualifiedIncome)
import Moneys
  ( TaxPayable,
    closeEnoughTo,
    makeFromInt,
    roundTaxPayable,
  )
import Test.Hspec
  ( Expectation,
    SpecWith,
    describe,
    it,
    shouldSatisfy,
  )
import FutureYearTestDataFromScala as FYTD
import KnownYearTestDataFromScala as KYTD

agreementWithScalaImplementationForKnownYearsSpec :: SpecWith ()
agreementWithScalaImplementationForKnownYearsSpec =
  describe "Federal.Calculator.taxDue" $
    it "matches outputs sampled from Scala implementation for known years" $ do
      let makeExpectation :: KYTD.TestCase -> Expectation
          makeExpectation tc@KYTD.TestCase {..} =
            let calculatedTaxDue =
                  roundTaxPayable
                    ( taxDueForKnownYear
                        year
                        birthDate
                        filingStatus
                        (dependents + 1)
                        socSec
                        ordinaryIncomeNonSS
                        qualifiedIncome
                        itemizedDeductions
                    )
             in do
                  --print tc
                  --taxDueForKnownYearDebug year filingStatus birthDate (dependents + 1) socSec ordinaryIncomeNonSS qualifiedIncome itemizedDeductions
                  calculatedTaxDue `shouldSatisfy` closeEnoughTo expectedFederalTax
          expectations = fmap makeExpectation KYTD.cases
       in () <$ sequence expectations

agreementWithScalaImplementationForFutureYearsSpec :: SpecWith ()
agreementWithScalaImplementationForFutureYearsSpec =
  describe "Federal.Calculator.taxDue" $
    it "matches outputs sampled from Scala implementation for future years" $ do
      let makeExpectation :: FYTD.TestCase -> Expectation
          makeExpectation tc@FYTD.TestCase {..} =
            let calculatedTaxDue =
                  roundTaxPayable
                    ( taxDueForFutureYear
                        regime
                        year
                        estimatedAnnualInflationFactor
                        birthDate
                        filingStatus
                        (dependents + 1)
                        socSec
                        ordinaryIncomeNonSS
                        qualifiedIncome
                        itemizedDeductions
                    )
             in do
                  --print tc
                  --taxDueForKnownYearDebug year filingStatus birthDate (dependents + 1) socSec ordinaryIncomeNonSS qualifiedIncome itemizedDeductions
                  calculatedTaxDue `shouldSatisfy` closeEnoughTo expectedFederalTax
          expectations = fmap makeExpectation FYTD.cases
       in () <$ sequence expectations
