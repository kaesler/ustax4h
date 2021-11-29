{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Federal.CalculatorSpec
  ( agreementWithMy2017ReturnSpec,
    agreementWithScalaImplementationSpec,
  )
where

import Federal.BoundRegime (bindRegime)
import Federal.Calculator (FederalTaxResults (..), makeCalculator, taxDue, taxDueDebug)
import Federal.Regime (Regime (..))
import qualified Kevin
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

agreementWithScalaImplementationSpec :: SpecWith ()
agreementWithScalaImplementationSpec =
  describe "Federal.Calculator.taxDue" $
    it "matches outputs sampled from Scala implementation" $ do
      let makeExpectation :: TestCase -> Expectation
          makeExpectation tc@TestCase {..} =
            let calculatedTaxDue =
                  roundHalfUp
                    ( taxDue
                        regime
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
                  -- print tc
                  -- taxDueDebug regime year filingStatus birthDate (dependents + 1) socSec ordinaryIncomeNonSS qualifiedIncome itemizedDeductions
                  calculatedTaxDue `shouldSatisfy` closeEnoughTo expectedFederalTax
          expectations = fmap makeExpectation TDFS.cases
       in () <$ sequence expectations

agreementWithMy2017ReturnSpec :: SpecWith ()
agreementWithMy2017ReturnSpec =
  describe "Federal.Calculator.taxDue" $
    it "agrees with my 2017 Federal return" $ do
      let regime = NonTrump
          year = 2017
          filingStatus = Kevin.filingStatus
          birthDate = Kevin.birthDate
          personalExemptions = Kevin.personalExemptions
          socSec = 0
          wages = 128270
          ordinaryDividends = 9196
          qualifiedDividends = 7686
          shortTermCapitalLoss = 2419
          hsaDeduction = 750
          itemizedDeductions = 22529
          totalIncome = wages + ordinaryDividends - shortTermCapitalLoss
          adjustedGrossIncome = totalIncome - hsaDeduction
          qualifiedIncome = qualifiedDividends
          ordinaryIncome = adjustedGrossIncome - qualifiedIncome
          boundRegime = bindRegime regime year filingStatus birthDate personalExemptions
          calc = makeCalculator boundRegime
          res = calc socSec ordinaryIncome qualifiedIncome itemizedDeductions
       in do
            -- print res
            taxOnOrdinaryIncome res `shouldSatisfy` closeEnoughTo 18246
            taxOnQualifiedIncome res `shouldSatisfy` closeEnoughTo 1153
