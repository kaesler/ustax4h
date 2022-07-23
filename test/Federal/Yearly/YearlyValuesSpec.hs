module Federal.Yearly.YearlyValuesSpec(
  averageThresholdChangeSpec
)

where

import Data.Maybe (fromJust)
import Federal.Yearly.YearlyValues
import Federal.Yearly.Type
import Moneys (roundHalfUp)

import Test.Hspec (SpecWith, describe, it, shouldBe)

averageThresholdChangeSpec :: SpecWith ()
averageThresholdChangeSpec =
  describe "YearlyValues.averageThresholdChangeOverPrevious produces expected values" $ do
    -- https://docs.google.com/spreadsheets/d/1Y_-LOViktEYW5hT-lY7XsU6vsPmCyg7s5sNkPxTKykI
    it "for 2018" $
      let asPercentage :: Double -> Double
          asPercentage factor = 
            (roundHalfUp ((factor - 1.0) * 10000)) / 100.0

      in do 
        asPercentage (fromJust (averageThresholdChangeOverPrevious 2018)) `shouldBe` 1.75
        asPercentage (fromJust (averageThresholdChangeOverPrevious 2019)) `shouldBe` 2.02
        asPercentage (fromJust (averageThresholdChangeOverPrevious 2020)) `shouldBe` 1.63
        asPercentage (fromJust (averageThresholdChangeOverPrevious 2021)) `shouldBe` 0.95
        asPercentage (fromJust (averageThresholdChangeOverPrevious 2022)) `shouldBe` 3.13

