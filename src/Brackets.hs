module Brackets
  ( Brackets,
    bracketWidth,
    fromPairs,
    inflateThresholds,
    ratesExceptTop,
    rateSuccessor,
    taxableIncomeToEndOfBracket,
    taxToEndOfBracket
  )
where

import Data.List.NonEmpty as NonEmpty (fromList, tail, last, takeWhile)
import Data.Map.NonEmpty (NEMap)
import qualified Data.Map.NonEmpty as NEMap
import Money.Money (IncomeThreshold, inflateThreshold, mkIncomeThreshold, TaxableIncome, thresholdDifference, TaxPayable, applyTaxRate)
import TaxRate (TaxRate)
import qualified Data.List as List
import Data.List.NonEmpty (toList)
import Data.Maybe (fromJust)

type Brackets r = NEMap r IncomeThreshold

fromPairs :: TaxRate r => [(Double, Integer)] -> (Double -> r) -> Brackets r
fromPairs pairs mkRate =
  let nePairs = NonEmpty.fromList pairs
      f (rateAsDouble, thresholdAsInteger) = (mkRate rateAsDouble, mkIncomeThreshold thresholdAsInteger)
      mappedPairs = f <$> nePairs
   in NEMap.fromList mappedPairs

inflateThresholds :: TaxRate r => Double -> Brackets r -> Brackets r
inflateThresholds factor = fmap (inflateThreshold factor)

rateSuccessor :: TaxRate r => r -> Brackets r -> Maybe r
rateSuccessor rate brackets =
  do
    let rates = NEMap.keys brackets
    let pairs = Prelude.zip (toList rates) (NonEmpty.tail rates)
    pair <- List.find (\p -> fst p == rate) pairs
    Just (snd pair)

ratesExceptTop :: TaxRate r => Brackets r -> [r]
ratesExceptTop brackets =
  let rates = NEMap.keys brackets
      topRate = NonEmpty.last rates
   in NonEmpty.takeWhile (/= topRate) rates

taxableIncomeToEndOfBracket :: TaxRate r => Brackets r -> r -> IncomeThreshold
taxableIncomeToEndOfBracket brackets bracketRate =
  let successorRate = fromJust (rateSuccessor bracketRate brackets)
   in fromJust (NEMap.lookup successorRate brackets)

bracketWidth :: TaxRate r => Brackets r -> r -> TaxableIncome
bracketWidth brackets rate =
  fromJust
    ( do
        threshold <- NEMap.lookup rate brackets
        successorRate <- rateSuccessor rate brackets
        successorThreshold <- NEMap.lookup successorRate brackets
        Just $ thresholdDifference threshold successorThreshold
    )

taxToEndOfBracket :: TaxRate r => Brackets r -> r -> TaxPayable
taxToEndOfBracket brackets bracketRate =
  let relevantRates = List.takeWhile (<= bracketRate) (ratesExceptTop brackets)
      bracketWidths = List.map (bracketWidth brackets) relevantRates
      pairs = relevantRates `zip` bracketWidths
      taxesDue = List.map taxForBracket pairs
        where
          taxForBracket (rate, width) = applyTaxRate rate width
   in mconcat taxesDue
