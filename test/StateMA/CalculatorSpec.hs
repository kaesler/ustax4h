{-# LANGUAGE NamedFieldPuns #-}

module StateMA.CalculatorSpec(
  stateMATaxCalculatorSpec
)
where

import StateMA.Calculator ( maStateTaxDue )
import Math ( roundHalfUp )
import MathInSpecs ( closeEnoughTo )
import Test.Hspec
    ( describe, it, shouldSatisfy, SpecWith, Expectation )
import TestDataFromScala as TDFS (TestCase (..), cases)

stateMATaxCalculatorSpec :: SpecWith ()
stateMATaxCalculatorSpec =
  describe "StateMA.maStateTaxDue" $
    it "matches outputs sampled from Scala implementation" $ do
      let makeExpectation :: TestCase -> Expectation
          makeExpectation tc@TestCase {age, dependents, filingStatus, socSec, ordinaryIncomeNonSS, qualifiedIncome, expectedFederalTax, expectedStateTax} =
            let calculatedTaxDue = roundHalfUp $ maStateTaxDue 2021 dependents filingStatus (ordinaryIncomeNonSS + qualifiedIncome)
             in do
                  -- print tc
                  calculatedTaxDue `shouldSatisfy` closeEnoughTo expectedStateTax
          expectations = fmap makeExpectation TDFS.cases
       in () <$ sequence expectations
