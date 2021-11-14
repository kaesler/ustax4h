module Federal.RegimeSpec
  ( futureEstimationSpec,
  )
where

import CommonTypes (FilingStatus (HeadOfHousehold), InflationEstimate (InflationEstimate))
import Federal.BoundRegime
import Federal.Regime
import qualified Kevin
import Test.Hspec (SpecWith, describe, it, shouldBe)

futureEstimationSpec :: SpecWith ()
futureEstimationSpec =
  describe "Federal.Regime.futureEstimated" $
    it "should behave as expected" $ do
      let before = bindRegime Trump 2021 HeadOfHousehold Kevin.birthDate Kevin.personalExemptions
          rate = 0.03 :: Double
          factor = 1.0 + rate
          after = futureEstimated before $ InflationEstimate 2022 rate
       in do
            -- print "Before:"
            -- print before
            -- print "After:"
            -- print after
            unadjustedStandardDeduction after `shouldBe` round (fromIntegral (unadjustedStandardDeduction before) * factor)
            perPersonExemption after `shouldBe` perPersonExemption before * factor
            standardDeduction before < standardDeduction after `shouldBe` True
            netDeduction before 15000 < netDeduction after 15000 `shouldBe` True
            netDeduction before 0 < netDeduction after 0 `shouldBe` True