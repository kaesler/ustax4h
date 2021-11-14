module Federal.QualifiedIncomeBracketSpec
  ( qualifiedIncomeBracketsSpec,
  )
where

import CommonTypes (FilingStatus (HeadOfHousehold, Single), Year)
import Federal.BoundRegime
import Federal.QualifiedIncome (applyQualifiedIncomeBrackets)
import Federal.Regime
import qualified Kevin
import Test.Hspec (SpecWith, describe, it, shouldBe)

theRegime :: Regime
theRegime = Trump

theYear :: Year
theYear = 2021

qualifiedIncomeBracketsSpec :: SpecWith ()
qualifiedIncomeBracketsSpec =
  -- TODO More here
  describe "Taxes.applyQualifiedBrackets" $
    it "never taxes zero income" $ do
      let brSingle = bindRegime theRegime theYear Single Kevin.birthDate 0
      applyQualifiedIncomeBrackets (qualifiedIncomeBrackets brSingle) 0.0 0.0 `shouldBe` 0.0
      let brHoH = bindRegime theRegime theYear HeadOfHousehold Kevin.birthDate 0
      applyQualifiedIncomeBrackets (qualifiedIncomeBrackets brHoH) 0.0 0.0 `shouldBe` 0.0
