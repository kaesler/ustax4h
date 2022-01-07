module Federal.RegimeSpec
  ( futureEstimationSpec,
  )
where

import CommonTypes (FilingStatus (HeadOfHousehold), InflationEstimate (InflationEstimate))
import Federal.BoundRegime
  ( BoundRegime (perPersonExemption, unadjustedStandardDeduction),
    bindRegime,
    futureEstimated,
    netDeduction,
    standardDeduction,
  )
import Federal.Regime (Regime (Trump))
import qualified Kevin
import Moneys (makeFromInt, mul, noMoney)
import Test.Hspec (SpecWith, describe, it, shouldBe)

futureEstimationSpec :: SpecWith ()
futureEstimationSpec =
  describe "Federal.Regime.futureEstimated" $
    it "should behave as expected" $ do
      let before = bindRegime Trump 2021 Kevin.birthDate HeadOfHousehold Kevin.personalExemptions
          rate = 0.03 :: Double
          factor = 1.0 + rate
          after = futureEstimated before $ InflationEstimate 2022 rate
       in do
            -- print "Before:"
            -- print before
            -- print "After:"
            -- print after
            unadjustedStandardDeduction after `shouldBe` (unadjustedStandardDeduction before) `mul` factor
            perPersonExemption after `shouldBe` perPersonExemption before `mul` factor
            standardDeduction before < standardDeduction after `shouldBe` True
            netDeduction before (makeFromInt 15000) < netDeduction after (makeFromInt 15000) `shouldBe` True
            netDeduction before noMoney < netDeduction after noMoney `shouldBe` True
