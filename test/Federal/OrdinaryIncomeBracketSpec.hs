module Federal.OrdinaryIncomeBracketSpec
  ( ordinaryIncomeBracketsSpec,
  )
where

import CommonTypes
  ( FilingStatus (..),
    OrdinaryIncome,
    SSRelevantOtherIncome,
    SocSec,
    Year,
  )
import Federal.Deductions
  ( StandardDeduction (StandardDeduction),
    standardDeduction,
  )
import Federal.OrdinaryIncome
  ( OrdinaryRate (..),
    applyOrdinaryIncomeBrackets,
    incomeToEndOfOrdinaryBracket,
    ordinaryRateAsFraction,
    ordinaryRatesExceptTop,
    taxToEndOfOrdinaryBracket,
    topRateOnOrdinaryIncome,
  )
import Math (roundHalfUp)
import MathInSpecs ()
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

year :: Year
year = 2021

genSocialSecurityBenefits :: Gen SocSec
genSocialSecurityBenefits = fmap fromInteger (elements [0 .. 50000])

genOrdinaryIncome :: Gen OrdinaryIncome
genOrdinaryIncome = fromInteger <$> elements [0 .. 100000]

genSsRelevantIncome :: Gen SSRelevantOtherIncome
genSsRelevantIncome = fmap fromInteger (elements [0 .. 100000])

genFilingStatus :: Gen FilingStatus
genFilingStatus = elements [Single, HeadOfHousehold]

genPair :: (Arbitrary a) => Gen (a, a)
genPair = do
  a1 <- arbitrary
  a2 <- arbitrary
  return (a1, a2)

genFsWithIncome :: Gen (FilingStatus, OrdinaryIncome)
genFsWithIncome = do
  fs <- genFilingStatus
  income <- genOrdinaryIncome
  return (fs, income)

prop_monotonic :: Property
prop_monotonic =
  forAll
    genCase
    ( \(fs, i1, i2) ->
        (i1 <= i2)
          == (applyOrdinaryIncomeBrackets year fs i1 <= applyOrdinaryIncomeBrackets year fs i2)
    )
  where
    genCase :: Gen (FilingStatus, OrdinaryIncome, OrdinaryIncome)
    genCase = do
      fs <- genFilingStatus
      i1 <- genOrdinaryIncome
      i2 <- genOrdinaryIncome
      return (fs, i1, i2)

prop_singlePaysMoreTax :: Property
prop_singlePaysMoreTax =
  forAll
    genOrdinaryIncome
    ( \income ->
        applyOrdinaryIncomeBrackets year Single income >= applyOrdinaryIncomeBrackets year HeadOfHousehold income
    )

prop_topRateIsNotExceeded :: Property
prop_topRateIsNotExceeded =
  forAll
    genFsWithIncome
    ( \(fs, income) ->
        let effectiveRate = applyOrdinaryIncomeBrackets year fs income / income
         in effectiveRate <= ordinaryRateAsFraction (topRateOnOrdinaryIncome year fs)
    )

prop_zeroTaxOnlyOnZeroIncome :: Property
prop_zeroTaxOnlyOnZeroIncome =
  forAll
    genFsWithIncome
    ( \(fs, income) ->
        applyOrdinaryIncomeBrackets year fs income /= 0 || income == 0
    )

assertCorrectTaxDueAtBracketBoundary :: FilingStatus -> OrdinaryRate -> Expectation
assertCorrectTaxDueAtBracketBoundary filingStatus bracketRate =
  let StandardDeduction deduction = standardDeduction year filingStatus
      income = incomeToEndOfOrdinaryBracket year filingStatus bracketRate
      taxableIncome = income - fromInteger deduction
      expectedTax = roundHalfUp $ taxToEndOfOrdinaryBracket year filingStatus bracketRate
      computedTax = roundHalfUp $ applyOrdinaryIncomeBrackets year filingStatus taxableIncome
   in do
        computedTax `shouldBe` expectedTax

assertCorrectTaxDueAtBracketBoundaries :: FilingStatus -> Expectation
assertCorrectTaxDueAtBracketBoundaries filingStatus =
  let brackets = ordinaryRatesExceptTop year filingStatus
      incomes = map (incomeToEndOfOrdinaryBracket year filingStatus) brackets
      expectedTaxes = map (taxToEndOfOrdinaryBracket year filingStatus) brackets
      StandardDeduction deduction = standardDeduction year filingStatus
      expectations = zipWith (curry taxDueIsAsExpected) incomes expectedTaxes
        where
          taxDueIsAsExpected :: (Double, Double) -> Expectation
          taxDueIsAsExpected (income, expectedTax) =
            let taxableIncome = income - fromInteger deduction
                computedTax = roundHalfUp $ applyOrdinaryIncomeBrackets year filingStatus taxableIncome
             in do
                  computedTax `shouldBe` roundHalfUp expectedTax
   in () <$ sequence expectations

ordinaryIncomeBracketsSpec :: SpecWith ()
ordinaryIncomeBracketsSpec =
  describe "Taxes.applyOrdinaryIncomeBrackets" $ do
    it "Never taxes zero income" $ do
      applyOrdinaryIncomeBrackets year Single 0.0 `shouldBe` 0.0
      applyOrdinaryIncomeBrackets year HeadOfHousehold 0.0 `shouldBe` 0.0

    it "Is monotonic" $ property prop_monotonic
    it "Single pays more tax than HeadOfHousehold" $ property prop_singlePaysMoreTax
    it "Top rate is top rate" $ property prop_topRateIsNotExceeded
    it "Computes expected tax at bracket boundaries" $ do
      assertCorrectTaxDueAtBracketBoundaries HeadOfHousehold
      assertCorrectTaxDueAtBracketBoundaries Single

      assertCorrectTaxDueAtBracketBoundary Single (OrdinaryRate 10)
      assertCorrectTaxDueAtBracketBoundary HeadOfHousehold (OrdinaryRate 10)
      assertCorrectTaxDueAtBracketBoundary Single (OrdinaryRate 12)
      assertCorrectTaxDueAtBracketBoundary HeadOfHousehold (OrdinaryRate 12)
      assertCorrectTaxDueAtBracketBoundary Single (OrdinaryRate 22)
      assertCorrectTaxDueAtBracketBoundary HeadOfHousehold (OrdinaryRate 22)
      assertCorrectTaxDueAtBracketBoundary Single (OrdinaryRate 24)
      assertCorrectTaxDueAtBracketBoundary HeadOfHousehold (OrdinaryRate 24)
      assertCorrectTaxDueAtBracketBoundary Single (OrdinaryRate 35)
      assertCorrectTaxDueAtBracketBoundary HeadOfHousehold (OrdinaryRate 35)
