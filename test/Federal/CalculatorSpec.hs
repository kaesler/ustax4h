{-# LANGUAGE RecordWildCards #-}

module Federal.CalculatorSpec
  ( agreementWithMy2017ReturnSpec,
    agreementWithScalaImplementationSpec,
  )
where

import Federal.BoundRegime (bindRegime)
import Federal.Calculator (FederalTaxResults (..), makeCalculator, taxDue, taxDueDebug)
import Federal.Regime (Regime (..))
import Federal.Types (OrdinaryIncome, QualifiedIncome)
import qualified Kevin
import Math (roundHalfUp)
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
      let regime = PreTrump
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
          ordinaryIncome = makeFromInt (adjustedGrossIncome - qualifiedIncome) :: OrdinaryIncome
          boundRegime = bindRegime regime year birthDate filingStatus personalExemptions
          calc = makeCalculator boundRegime
          res = calc (makeFromInt socSec) ordinaryIncome (makeFromInt qualifiedIncome :: QualifiedIncome) (makeFromInt itemizedDeductions)
       in do
            -- print res
            taxOnOrdinaryIncome res `shouldSatisfy` closeEnoughTo (makeFromInt 18246 :: TaxPayable)
            taxOnQualifiedIncome res `shouldSatisfy` closeEnoughTo (makeFromInt 1153 :: TaxPayable)
