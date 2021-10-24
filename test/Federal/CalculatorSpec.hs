{-# LANGUAGE NamedFieldPuns #-}

module Federal.CalculatorSpec (federalTaxCalculatorSpec) where

import Federal.Calculator (federalTaxDue)
import Math (roundHalfUp)
import MathInSpecs (closeEnoughTo)
import Test.Hspec
  ( Expectation,
    SpecWith,
    describe,
    it,
    shouldSatisfy,
  )
import TestDataFromScala as TDFS (TestCase (..), cases)

federalTaxCalculatorSpec :: SpecWith ()
federalTaxCalculatorSpec =
  describe "Taxes.federalTaxDue" $
    it "matches outputs sampled from Scala implementation" $ do
      let makeExpectation :: TestCase -> Expectation
          makeExpectation TestCase {age, dependents, filingStatus, socSec, ordinaryIncomeNonSS, qualifiedIncome, expectedFederalTax} =
            let calculatedTaxDue = roundHalfUp (federalTaxDue 2021 filingStatus socSec ordinaryIncomeNonSS qualifiedIncome)
             in calculatedTaxDue `shouldSatisfy` closeEnoughTo expectedFederalTax
          expectations = fmap makeExpectation TDFS.cases
       in () <$ sequence expectations
