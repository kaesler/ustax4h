module Federal.OrdinaryBracketsSpec
  ( ordinaryBracketsSpec,
  )
where

import CommonTypes
  ( BirthDate,
    FilingStatus (..),
    Year,
  )
import Data.Time (fromGregorian)
import Federal.BoundRegime
  ( BoundRegime (ordinaryBrackets),
    boundRegimeForKnownYear,
  )
import Federal.FederalTaxRate (FederalTaxRate, mkFederalTaxRate)
import Federal.OrdinaryBrackets
  ( OrdinaryBrackets,
    ordinaryRatesExceptTop,
    taxToEndOfOrdinaryBracket,
    taxableIncomeToEndOfOrdinaryBracket,
  )
import qualified Federal.TaxFunctions as TFS
import Federal.Types (SSRelevantOtherIncome, SocSec)
import Moneys
  ( TaxPayable,
    TaxableIncome,
    makeFromInt,
    noMoney,
    roundTaxPayable,
  )
import Test.Hspec (Expectation, SpecWith, describe, it, shouldBe)
import Test.Hspec.QuickCheck ()
import Test.QuickCheck
  ( Arbitrary (arbitrary),
    Gen,
    Property,
    Testable (property),
    elements,
    forAll,
  )

theYear :: Year
theYear = 2021

theBirthDate :: BirthDate
theBirthDate = fromGregorian 1955 10 2

thePersonalExemptions :: Int
thePersonalExemptions = 2

ordinaryBracketsFor :: FilingStatus -> OrdinaryBrackets
ordinaryBracketsFor filingStatus =
  let br = boundRegimeForKnownYear theYear filingStatus
   in ordinaryBrackets br

genSocialSecurityBenefits :: Gen SocSec
genSocialSecurityBenefits = fmap makeFromInt (elements [0 .. 50000])

genTaxableIncome :: Gen TaxableIncome
genTaxableIncome = makeFromInt <$> elements [0 .. 100000]

genSsRelevantIncome :: Gen SSRelevantOtherIncome
genSsRelevantIncome = fmap makeFromInt (elements [0 .. 100000])

genFilingStatus :: Gen FilingStatus
genFilingStatus = elements [Single, HeadOfHousehold]

genPair :: (Arbitrary a) => Gen (a, a)
genPair = do
  a1 <- arbitrary
  a2 <- arbitrary
  return (a1, a2)

genFsWithIncome :: Gen (FilingStatus, TaxableIncome)
genFsWithIncome = do
  fs <- genFilingStatus
  income <- genTaxableIncome
  return (fs, income)

prop_monotonic :: Property
prop_monotonic =
  forAll
    genCase
    ( \(fs, i1, i2) ->
        (i1 <= i2)
          == ( TFS.taxDueOnOrdinaryIncome (ordinaryBracketsFor fs) i1
                 <= TFS.taxDueOnOrdinaryIncome (ordinaryBracketsFor fs) i2
             )
    )
  where
    genCase :: Gen (FilingStatus, TaxableIncome, TaxableIncome)
    genCase = do
      fs <- genFilingStatus
      i1 <- genTaxableIncome
      i2 <- genTaxableIncome
      return (fs, i1, i2)

prop_singlePaysMoreTax :: Property
prop_singlePaysMoreTax =
  forAll
    genTaxableIncome
    ( \income ->
        TFS.taxDueOnOrdinaryIncome (ordinaryBracketsFor Single) income
          >= TFS.taxDueOnOrdinaryIncome (ordinaryBracketsFor HeadOfHousehold) income
    )

{- prop_topRateIsNotExceeded :: Property
prop_topRateIsNotExceeded =
  forAll
    genFsWithIncome
    ( \(fs, income) ->
        let brackets = ordinaryBracketsFor fs
            effectiveRate = TFS.taxDueOnOrdinaryIncome brackets income / income
         in effectiveRate <= ordinaryRateAsFraction (topRateOnOrdinaryIncome brackets)
    )
 -}
prop_zeroTaxOnlyOnZeroIncome :: Property
prop_zeroTaxOnlyOnZeroIncome =
  forAll
    genFsWithIncome
    ( \(fs, income) ->
        TFS.taxDueOnOrdinaryIncome (ordinaryBracketsFor fs) income /= noMoney || income == noMoney
    )

assertCorrectTaxDueAtBracketBoundary :: FilingStatus -> FederalTaxRate -> Expectation
assertCorrectTaxDueAtBracketBoundary filingStatus bracketRate =
  let brackets = ordinaryBracketsFor filingStatus
      taxableIncome = taxableIncomeToEndOfOrdinaryBracket brackets bracketRate
      expectedTax = roundTaxPayable $ taxToEndOfOrdinaryBracket brackets bracketRate
      computedTax = roundTaxPayable $ TFS.taxDueOnOrdinaryIncome brackets taxableIncome
   in do
        computedTax `shouldBe` expectedTax

assertCorrectTaxDueAtBracketBoundaries :: FilingStatus -> Expectation
assertCorrectTaxDueAtBracketBoundaries filingStatus =
  let brackets = ordinaryBracketsFor filingStatus
      rates = ordinaryRatesExceptTop brackets
      incomes = map (taxableIncomeToEndOfOrdinaryBracket brackets) rates
      expectedTaxes = map (taxToEndOfOrdinaryBracket brackets) rates
      expectations = zipWith (curry taxDueIsAsExpected) incomes expectedTaxes
        where
          taxDueIsAsExpected :: (TaxableIncome, TaxPayable) -> Expectation
          taxDueIsAsExpected (taxableIncome, expectedTax) =
            let computedTax = roundTaxPayable $ TFS.taxDueOnOrdinaryIncome brackets taxableIncome
             in do
                  computedTax `shouldBe` roundTaxPayable expectedTax
   in () <$ sequence expectations

ordinaryBracketsSpec :: SpecWith ()
ordinaryBracketsSpec =
  describe "TaxFunction from OrdinaryBrackets" $ do
    it "Never taxes zero income" $ do
      TFS.taxDueOnOrdinaryIncome (ordinaryBracketsFor Single) noMoney `shouldBe` noMoney
      TFS.taxDueOnOrdinaryIncome (ordinaryBracketsFor HeadOfHousehold) noMoney `shouldBe` noMoney

    it "Is monotonic" $ property prop_monotonic
    it "Single pays more tax than HeadOfHousehold" $ property prop_singlePaysMoreTax
    --it "Top rate is top rate" $ property prop_topRateIsNotExceeded
    it "Computes expected tax at bracket boundaries" $ do
      assertCorrectTaxDueAtBracketBoundaries HeadOfHousehold
      assertCorrectTaxDueAtBracketBoundaries Single

      assertCorrectTaxDueAtBracketBoundary Single (mkFederalTaxRate 0.10)
      assertCorrectTaxDueAtBracketBoundary HeadOfHousehold (mkFederalTaxRate 0.10)
      assertCorrectTaxDueAtBracketBoundary Single (mkFederalTaxRate 0.12)
      assertCorrectTaxDueAtBracketBoundary HeadOfHousehold (mkFederalTaxRate 0.12)
      assertCorrectTaxDueAtBracketBoundary Single (mkFederalTaxRate 0.22)
      assertCorrectTaxDueAtBracketBoundary HeadOfHousehold $ mkFederalTaxRate 0.22
      assertCorrectTaxDueAtBracketBoundary Single $ mkFederalTaxRate 0.24
      assertCorrectTaxDueAtBracketBoundary HeadOfHousehold $ mkFederalTaxRate 0.24
      assertCorrectTaxDueAtBracketBoundary Single $ mkFederalTaxRate 0.35
      assertCorrectTaxDueAtBracketBoundary HeadOfHousehold $ mkFederalTaxRate 0.35
