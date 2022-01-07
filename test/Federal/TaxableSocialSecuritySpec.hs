module Federal.TaxableSocialSecuritySpec
  ( taxableSocialSecuritySpec,
  )
where

import CommonTypes (FilingStatus (Single))
import qualified Federal.TaxableSocialSecurity as TSS
import Moneys (makeFromInt, noMoney)
import Test.Hspec (SpecWith, describe, it, shouldBe)

taxableSocialSecuritySpec :: SpecWith ()
taxableSocialSecuritySpec =
  describe "Federal.TaxableSocialSecurit.taxableSocialSecurity" $ do
    it "Untaxable 1" $
      TSS.amountTaxable Single (makeFromInt 50000) noMoney `shouldBe` noMoney
    it "Untaxable 2" $
      TSS.amountTaxable Single (makeFromInt 40000) (makeFromInt 5000) `shouldBe` noMoney
    it "Top of middle tier 1" $
      TSS.amountTaxable Single (makeFromInt 68000) noMoney `shouldBe` makeFromInt 4500
    it "Top of middle tier 2" $
      TSS.amountTaxable Single (makeFromInt 28000) (makeFromInt 20000) `shouldBe` makeFromInt 4500
    it "Example 1 from Pub 915" $
      TSS.amountTaxable Single (makeFromInt 5980) (makeFromInt 28900) `shouldBe` makeFromInt 2990
    it "Jackson Example from Pub 915" $
      TSS.amountTaxable Single (makeFromInt 11000) (makeFromInt 25500) `shouldBe` makeFromInt 3000
    it "Example like I will face" $
      TSS.amountTaxable Single (makeFromInt 49000) (makeFromInt 17000) `shouldBe` makeFromInt 10875
