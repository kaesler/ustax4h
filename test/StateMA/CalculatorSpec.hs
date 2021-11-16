{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

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
          makeExpectation TestCase {..} =
            let calculatedTaxDue =
                  roundHalfUp
                    ( Calc.taxDue
                        year
                        dependents
                        filingStatus
                        (ordinaryIncomeNonSS + qualifiedIncome)
                    )
             in do
                  calculatedTaxDue `shouldSatisfy` closeEnoughTo expectedStateTax
          expectations = fmap makeExpectation TDFS.cases
       in () <$ sequence expectations
