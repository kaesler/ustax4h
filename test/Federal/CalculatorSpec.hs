{-# LANGUAGE RecordWildCards #-}

module Federal.CalculatorSpec
  ( agreementWithScalaImplementationSpec,
  )
where

import Federal.Calculator (FederalTaxResults (..), makeCalculator, taxDueForKnownYear, taxDueForKnownYearDebug)
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
import TestDataFromScala as TDFS (TestCase (..), cases)

agreementWithScalaImplementationSpec :: SpecWith ()
agreementWithScalaImplementationSpec =
  describe "Federal.Calculator.taxDue" $
    it "matches outputs sampled from Scala implementation" $ do
      let makeExpectation :: TestCase -> Expectation
          makeExpectation tc@TestCase {..} =
            let calculatedTaxDue =
                  roundTaxPayable
                    ( taxDueForKnownYear
                        year
                        filingStatus
                        birthDate
                        (dependents + 1)
                        socSec
                        ordinaryIncomeNonSS
                        qualifiedIncome
                        itemizedDeductions
                    )
             in do
                  print tc
                  taxDueForKnownYearDebug year filingStatus birthDate (dependents + 1) socSec ordinaryIncomeNonSS qualifiedIncome itemizedDeductions
                  calculatedTaxDue `shouldSatisfy` closeEnoughTo expectedFederalTax
          expectations = fmap makeExpectation TDFS.cases
       in () <$ sequence expectations
