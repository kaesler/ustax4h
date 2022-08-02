module Federal.RegimeSpec
  ( futureEstimationSpec,
  )
where

import CommonTypes (BirthDate, FilingStatus (HeadOfHousehold))
import Data.Time
import Federal.BoundRegime
  ( BoundRegime (perPersonExemption, unadjustedStandardDeduction),
    boundRegimeForKnownYear,
    netDeduction,
    standardDeduction,
    withEstimatedNetInflationFactor,
  )
import Moneys (makeFromInt, mul, noMoney)
import Test.Hspec (SpecWith, describe, it, shouldBe)

theBirthDate :: BirthDate
theBirthDate = fromGregorian 1955 10 2

futureEstimationSpec :: SpecWith ()
futureEstimationSpec =
  describe "Federal.Regime.futureEstimated" $
    it "should behave as expected" $ do
      let before = boundRegimeForKnownYear 2021 HeadOfHousehold
          rate = 0.03 :: Double
          factor = 1.0 + rate
          after = withEstimatedNetInflationFactor 2022 factor before
       in do
            -- print "Before:"
            -- print before
            -- print "After:"
            -- print after
            unadjustedStandardDeduction after `shouldBe` (unadjustedStandardDeduction before) `mul` factor
            perPersonExemption after `shouldBe` perPersonExemption before `mul` factor
            standardDeduction before theBirthDate < standardDeduction after theBirthDate `shouldBe` True
            netDeduction before theBirthDate 0 (makeFromInt 15000) < netDeduction after theBirthDate 0 (makeFromInt 15000) `shouldBe` True
            netDeduction before theBirthDate 0 noMoney < netDeduction after theBirthDate 0 noMoney `shouldBe` True
