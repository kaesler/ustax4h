import Test.Hspec
import Test.Hspec.QuickCheck
import Taxes

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
      applyOrdinaryIncomeBrackets HeadOfHousehold  0.0 `shouldBe` 0.0

  describe "applyQualifiedBrackets" $ do
    it "never tax zero" $ do
      applyQualifiedBrackets Single 0.0 0.0 `shouldBe` 0.0  
      applyQualifiedBrackets HeadOfHousehold 0.0 0.0 `shouldBe` 0.0