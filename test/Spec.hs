{-# LANGUAGE NamedFieldPuns #-}

import System.IO.Unsafe (unsafePerformIO)
import Taxes
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import TestDataFromScala as TDFS (TestCase (..), cases)

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

closeEnoughTo :: Double -> Double -> Bool
closeEnoughTo x y = abs (x - y) <= 1.0

main :: IO ()
main = hspec $ do
  describe "Taxes.taxableSocialSecurity" $ do
    it "Untaxable 1" $
      taxableSocialSecurity Single 50000.0 0.0 `shouldBe` 0.0
    it "Untaxable 2" $
      taxableSocialSecurity Single 40000.0 5000.0 `shouldBe` 0.0
    it "Top of middle tier 1" $
      taxableSocialSecurity Single 68000.0 0.0 `shouldBe` 4500.0
    it "Top of middle tier 2" $
      taxableSocialSecurity Single 28000.0 20000.0 `shouldBe` 4500.0
    it "Example 1 from Pub 915" $
      taxableSocialSecurity Single 5980.0 28900.0 `shouldBe` 2990.0
    it "Jackson Example from Pub 915" $
      taxableSocialSecurity Single 11000.0 25500.0 `shouldBe` 3000.0
    it "Example like I will face" $
      taxableSocialSecurity Single 49000.0 17000.0 `shouldBe` 10875.0

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

  -- TODO More here
  describe "Taxes.applyQualifiedBrackets" $
    it "never taxes zero income" $ do
      applyQualifiedIncomeBrackets Single 0.0 0.0 `shouldBe` 0.0
      applyQualifiedIncomeBrackets HeadOfHousehold 0.0 0.0 `shouldBe` 0.0

  describe "Taxes.federalTaxDue" $
    it "matches outputs sampled from Scala implementation" $ do
      let makeExpectation :: TestCase -> Expectation
          makeExpectation TestCase {age, dependents, filingStatus, socSec, ordinaryIncomeNonSS, qualifiedIncome, expectedFederalTax} =
            let calculatedTaxDue = roundHalfUp (federalTaxDue 2021 filingStatus socSec ordinaryIncomeNonSS qualifiedIncome)
             in calculatedTaxDue `shouldSatisfy` closeEnoughTo expectedFederalTax
          expectations = fmap makeExpectation TDFS.cases
       in () <$ sequence expectations

  describe "Taxes.stateTaxDue" $
    it "matches outputs sampled from Scala implementation" $ do
      let makeExpectation :: TestCase -> Expectation
          makeExpectation tc@TestCase {age, dependents, filingStatus, socSec, ordinaryIncomeNonSS, qualifiedIncome, expectedFederalTax, expectedStateTax} =
            let calculatedTaxDue = roundHalfUp $ maStateTaxDue 2021 dependents filingStatus (ordinaryIncomeNonSS + qualifiedIncome)
             in do
                  -- print tc
                  calculatedTaxDue `shouldSatisfy` closeEnoughTo expectedStateTax
          expectations = fmap makeExpectation TDFS.cases
       in () <$ sequence expectations
