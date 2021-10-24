module Federal.QualifiedIncomeBracketSpec
  ( qualifiedIncomeBracketsSpec,
  )
where

import CommonTypes
import Federal.QualifiedIncome (applyQualifiedIncomeBrackets)
import Test.Hspec (SpecWith, describe, it, shouldBe)

qualifiedIncomeBracketsSpec :: SpecWith ()
qualifiedIncomeBracketsSpec =
  -- TODO More here
  describe "Taxes.applyQualifiedBrackets" $
    it "never taxes zero income" $ do
      applyQualifiedIncomeBrackets Single 0.0 0.0 `shouldBe` 0.0
      applyQualifiedIncomeBrackets HeadOfHousehold 0.0 0.0 `shouldBe` 0.0
