{-# LANGUAGE NamedFieldPuns #-}

module StateMA.CalculatorSpec
  ( stateMATaxCalculatorSpec,
  )
where

import Math (roundHalfUp)
import MathInSpecs (closeEnoughTo)
import qualified StateMA.Calculator as Calc
import Test.Hspec
  ( Expectation,
    SpecWith,
    describe,
    it,
    shouldSatisfy,
  )
import TestDataFromScala as TDFS (TestCase (..), cases)

stateMATaxCalculatorSpec :: SpecWith ()
stateMATaxCalculatorSpec =
  describe "StateMA.taxDue" $
    it "matches outputs sampled from Scala implementation" $ do
      let makeExpectation :: TestCase -> Expectation
          makeExpectation tc@TestCase {age, dependents, filingStatus, socSec, ordinaryIncomeNonSS, qualifiedIncome, expectedFederalTax, expectedStateTax} =
            let calculatedTaxDue = roundHalfUp $ Calc.taxDue 2021 dependents filingStatus (ordinaryIncomeNonSS + qualifiedIncome)
             in do
                  calculatedTaxDue `shouldSatisfy` closeEnoughTo expectedStateTax
          expectations = fmap makeExpectation TDFS.cases
       in () <$ sequence expectations
