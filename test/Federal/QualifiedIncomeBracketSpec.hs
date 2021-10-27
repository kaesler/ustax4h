module Federal.QualifiedIncomeBracketSpec
  ( qualifiedIncomeBracketsSpec,
  )
where

import CommonTypes ( FilingStatus(HeadOfHousehold, Single), Year)
import Federal.QualifiedIncome (applyQualifiedIncomeBrackets)
import Federal.Regime
import Test.Hspec (SpecWith, describe, it, shouldBe)
import qualified Kevin

year :: Year
year = 2021

qualifiedIncomeBracketsSpec :: SpecWith ()
qualifiedIncomeBracketsSpec =
  -- TODO More here
  describe "Taxes.applyQualifiedBrackets" $
    it "never taxes zero income" $ do
      let brSingle = bindRegime Trump 2021 Single Kevin.birthDate 0
      applyQualifiedIncomeBrackets (qualifiedIncomeBrackets brSingle) 0.0 0.0 `shouldBe` 0.0
      let brHoH = bindRegime Trump 2021 HeadOfHousehold Kevin.birthDate 0
      applyQualifiedIncomeBrackets (qualifiedIncomeBrackets brHoH) 0.0 0.0 `shouldBe` 0.0
