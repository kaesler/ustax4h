module Federal.QualifiedBracketsSpec
  ( qualifiedBracketsSpec,
  )
where

import CommonTypes (FilingStatus (HeadOfHousehold, Single), Year)
import Federal.BoundRegime
  ( BoundRegime (qualifiedBrackets),
    bindRegime,
  )
import Federal.Regime (Regime (Trump))
import qualified Federal.TaxFunctions as TFS
import qualified Kevin
import Moneys (noMoney)
import Test.Hspec (SpecWith, describe, it, shouldBe)

theRegime :: Regime
theRegime = Trump

theYear :: Year
theYear = 2021

qualifiedBracketsSpec :: SpecWith ()
qualifiedBracketsSpec =
  -- TODO More here
  describe "TaxeFunction from QualifiedBrackets" $
    it "never taxes zero income" $ do
      let brSingle = bindRegime theRegime theYear Kevin.birthDate Single 0
      TFS.taxDueOnQualifiedIncome (qualifiedBrackets brSingle) noMoney noMoney `shouldBe` noMoney
      let brHoH = bindRegime theRegime theYear Kevin.birthDate HeadOfHousehold 0
      TFS.taxDueOnQualifiedIncome (qualifiedBrackets brHoH) noMoney noMoney `shouldBe` noMoney
