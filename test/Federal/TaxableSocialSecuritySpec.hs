module Federal.TaxableSocialSecuritySpec
  ( taxableSocialSecuritySpec,
  )
where

import CommonTypes (FilingStatus (Single))
import qualified Federal.TaxableSocialSecurity as TSS
import Test.Hspec (SpecWith, describe, it, shouldBe)

taxableSocialSecuritySpec :: SpecWith ()
taxableSocialSecuritySpec =
  describe "Federal.TaxableSocialSecurit.taxableSocialSecurity" $ do
    it "Untaxable 1" $
      TSS.amountTaxable Single 50000.0 0.0 `shouldBe` 0.0
    it "Untaxable 2" $
      TSS.amountTaxable Single 40000.0 5000.0 `shouldBe` 0.0
    it "Top of middle tier 1" $
      TSS.amountTaxable Single 68000.0 0.0 `shouldBe` 4500.0
    it "Top of middle tier 2" $
      TSS.amountTaxable Single 28000.0 20000.0 `shouldBe` 4500.0
    it "Example 1 from Pub 915" $
      TSS.amountTaxable Single 5980.0 28900.0 `shouldBe` 2990.0
    it "Jackson Example from Pub 915" $
      TSS.amountTaxable Single 11000.0 25500.0 `shouldBe` 3000.0
    it "Example like I will face" $
      TSS.amountTaxable Single 49000.0 17000.0 `shouldBe` 10875.0
