{-# LANGUAGE RecordWildCards #-}

module StateMA.CalculatorSpec
  ( stateMATaxCalculatorSpec,
  )
where

import Math (roundHalfUp)
import Moneys (closeEnoughTo, roundTaxPayable)
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
          makeExpectation tc@TestCase {..} =
            let calculatedTaxDue =
                  roundTaxPayable
                    ( Calc.taxDue
                        year
                        birthDate
                        dependents
                        filingStatus
                        (ordinaryIncomeNonSS <> qualifiedIncome)
                    )
             in do
                  -- print tc
                  -- print calculatedTaxDue
                  calculatedTaxDue `shouldSatisfy` closeEnoughTo expectedStateTax
          expectations = fmap makeExpectation TDFS.cases
       in () <$ sequence expectations
