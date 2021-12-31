module Brackets
  ( Brackets,
    fromPairs,
    inflateThresholds,
  )
where

import Data.List.NonEmpty as NonEmpty (fromList)
import Data.Map.NonEmpty (NEMap)
import qualified Data.Map.NonEmpty as NEMap
import Money.Money (IncomeThreshold, inflateThreshold, mkIncomeThreshold)
import TaxRate (TaxRate)

type Brackets r = NEMap r IncomeThreshold

fromPairs :: TaxRate r => [(Double, Integer)] -> (Double -> r) -> Brackets r
fromPairs pairs mkRate =
  let nePairs = NonEmpty.fromList pairs
      f (rateAsDouble, thresholdAsInteger) = (mkRate rateAsDouble, mkIncomeThreshold thresholdAsInteger)
      mappedPairs = f <$> nePairs
   in NEMap.fromList mappedPairs

inflateThresholds :: TaxRate r => Double -> Brackets r -> Brackets r
inflateThresholds factor = fmap (inflateThreshold factor)
