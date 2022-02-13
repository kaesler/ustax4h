module Federal.QualifiedBracketsSpec
  ( qualifiedBracketsSpec,
  )
where

import CommonTypes (BirthDate, FilingStatus (HeadOfHousehold, Single), Year)
import Data.Time
import Federal.BoundRegime
  ( BoundRegime (qualifiedBrackets),
    boundRegimeForKnownYear,
  )
import qualified Federal.TaxFunctions as TFS
import Moneys (noMoney)
import Test.Hspec (SpecWith, describe, it, shouldBe)

theYear :: Year
theYear = 2021

theBirthDate :: BirthDate
theBirthDate = fromGregorian 1955 10 2

qualifiedBracketsSpec :: SpecWith ()
qualifiedBracketsSpec =
  describe "TaxeFunction from QualifiedBrackets" $
    it "never taxes zero income" $ do
      let brSingle = boundRegimeForKnownYear theYear theBirthDate Single 0
      TFS.taxDueOnQualifiedIncome (qualifiedBrackets brSingle) noMoney noMoney `shouldBe` noMoney
      let brHoH = boundRegimeForKnownYear theYear theBirthDate HeadOfHousehold 0
      TFS.taxDueOnQualifiedIncome (qualifiedBrackets brHoH) noMoney noMoney `shouldBe` noMoney
