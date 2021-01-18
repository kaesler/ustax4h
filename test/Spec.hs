import System.IO.Unsafe (unsafePerformIO)
import Taxes
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

genSocialSecurityBenefits :: Gen SocialSecurityBenefits
genSocialSecurityBenefits = fmap fromIntegral (elements [0 .. 50000])

genTaxableOrdinaryIncome :: Gen TaxableOrdinaryIncome
genTaxableOrdinaryIncome = fromIntegral <$> elements [0 .. 100000]

genSsRelevantIncome :: Gen SSRelevantIncome
genSsRelevantIncome = fmap fromIntegral (elements [0 .. 100000])

genFilingStatus :: Gen FilingStatus
genFilingStatus = elements [Single, HeadOfHousehold]

genPair :: (Arbitrary a) => Gen (a, a)
genPair = do
  a1 <- arbitrary
  a2 <- arbitrary
  return (a1, a2)

genFsWithIncome :: Gen (FilingStatus, TaxableOrdinaryIncome)
genFsWithIncome = do
  fs <- genFilingStatus
  income <- genTaxableOrdinaryIncome
  return (fs, income)

prop_monotonic :: Property
prop_monotonic =
  forAll
    genCase
    ( \(fs, i1, i2) ->
        (i1 <= i2)
          == (applyOrdinaryIncomeBrackets fs i1 <= applyOrdinaryIncomeBrackets fs i2)
    )
  where
    genCase :: Gen (FilingStatus, TaxableOrdinaryIncome, TaxableOrdinaryIncome)
    genCase = do
      fs <- genFilingStatus
      i1 <- genTaxableOrdinaryIncome
      i2 <- genTaxableOrdinaryIncome
      return (fs, i2, i2)

prop_singlePaysMoreTax :: Property
prop_singlePaysMoreTax =
  forAll
    genTaxableOrdinaryIncome
    ( \income ->
        applyOrdinaryIncomeBrackets Single income >= applyOrdinaryIncomeBrackets HeadOfHousehold income
    )

prop_topRateIsNotExceeded :: Property
prop_topRateIsNotExceeded =
  forAll
    genFsWithIncome
    ( \(fs, income) ->
        let effectiveRate = applyOrdinaryIncomeBrackets fs income / income
         in effectiveRate <= ordinaryRateAsFraction (topRateOnOrdinaryIncome fs)
    )

assertIt :: (FilingStatus, TaxableOrdinaryIncome) -> Bool
assertIt (fs, income) =
  let tax = applyOrdinaryIncomeBrackets fs income
      effectiveRate = tax / income
      bottomRate = ordinaryRateAsFraction (bottomRateOnOrdinaryIncome fs)
   in effectiveRate >= bottomRate

prop_zeroTaxOnlyOnZeroIncome :: Property
prop_zeroTaxOnlyOnZeroIncome =
  forAll
    genFsWithIncome
    ( \(fs, income) ->
        applyOrdinaryIncomeBrackets fs income /= 0 || income == 0
    )

main :: IO ()
main = hspec $ do
  describe "taxableSocialSecurity" $ do
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

  describe "applyOrdinaryIncomeBrackets" $ do
    it "never tax zero" $ do
      applyOrdinaryIncomeBrackets Single 0.0 `shouldBe` 0.0
      applyOrdinaryIncomeBrackets HeadOfHousehold 0.0 `shouldBe` 0.0

    it "applyOrdinaryIncomeBrackets is monotonic" $ property prop_monotonic
    it "Single pays more tax than HeadOfHousehold" $ property prop_singlePaysMoreTax
    it "top rate is top rate" $ property prop_topRateIsNotExceeded

  describe "applyQualifiedBrackets" $ do
    it "never tax zero" $ do
      applyQualifiedBrackets Single 0.0 0.0 `shouldBe` 0.0
      applyQualifiedBrackets HeadOfHousehold 0.0 0.0 `shouldBe` 0.0