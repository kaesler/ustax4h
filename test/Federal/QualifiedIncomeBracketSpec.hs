module Federal.QualifiedIncomeBracketSpec
  ( qualifiedIncomeBracketsSpec,
  )
where

import CommonTypes ( FilingStatus(HeadOfHousehold, Single), Year)
import Federal.QualifiedIncome (qualifiedIncomeBracketsFor, applyQualifiedIncomeBrackets)
import Test.Hspec (SpecWith, describe, it, shouldBe)

year :: Year
year = 2021

qualifiedIncomeBracketsSpec :: SpecWith ()
qualifiedIncomeBracketsSpec =
  -- TODO More here
  describe "Taxes.applyQualifiedBrackets" $
    it "never taxes zero income" $ do
      applyQualifiedIncomeBrackets (qualifiedIncomeBracketsFor year Single) 0.0 0.0 `shouldBe` 0.0
      applyQualifiedIncomeBrackets (qualifiedIncomeBracketsFor year HeadOfHousehold) 0.0 0.0 `shouldBe` 0.0
