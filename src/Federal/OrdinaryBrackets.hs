module Federal.OrdinaryBrackets
  ( inflateThresholds,
    fromPairs,
    rateSuccessor,
  )
where

import Brackets (Brackets)
import qualified Data.Containers.NonEmpty as NEMap
import qualified Data.List as List
import Data.List.NonEmpty (toList)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.NonEmpty as NEMap
import Data.Maybe (fromJust)
import Federal.FederalTaxRate (FederalTaxRate, mkFederalTaxRate)
import Money.Money (IncomeThreshold, TaxPayable, TaxableIncome, applyTaxRate, inflateThreshold, mkIncomeThreshold, thresholdDifference)

newtype OrdinaryBrackets = OrdinaryBrackets (Brackets FederalTaxRate)

-- TODO: do we need all these?

inflateThresholds :: OrdinaryBrackets -> Double -> OrdinaryBrackets
inflateThresholds (OrdinaryBrackets brackets) factor =
  OrdinaryBrackets
    ( fmap (inflateThreshold factor) brackets
    )

fromPairs :: [(Double, Integer)] -> OrdinaryBrackets
fromPairs pairs =
  let f (rateAsDouble, startAsInt) = (mkFederalTaxRate rateAsDouble, mkIncomeThreshold startAsInt)
      mappedPairs = f <$> NonEmpty.fromList pairs
      brs = NEMap.fromList mappedPairs
   in OrdinaryBrackets brs

rateSuccessor :: OrdinaryBrackets -> FederalTaxRate -> Maybe FederalTaxRate
rateSuccessor (OrdinaryBrackets brackets) rate =
  do
    let rates = NEMap.keys brackets
    let pairs = Prelude.zip (toList rates) (NonEmpty.tail rates)
    pair <- List.find (\p -> fst p == rate) pairs
    Just (snd pair)

ordinaryRatesExceptTop :: OrdinaryBrackets -> [FederalTaxRate]
ordinaryRatesExceptTop (OrdinaryBrackets brackets) =
  let rates = NEMap.keys brackets
      topRate = NonEmpty.last rates
   in NonEmpty.takeWhile (/= topRate) rates

taxableIncomeToEndOfOrdinaryBracket :: OrdinaryBrackets -> FederalTaxRate -> IncomeThreshold
taxableIncomeToEndOfOrdinaryBracket os@(OrdinaryBrackets brackets) bracketRate =
  let successorRate = fromJust (rateSuccessor os bracketRate)
   in fromJust (NEMap.lookup successorRate brackets)

ordinaryIncomeBracketWidth :: OrdinaryBrackets -> FederalTaxRate -> TaxableIncome
ordinaryIncomeBracketWidth obs@(OrdinaryBrackets brackets) rate =
  fromJust
    ( do
        threshold <- NEMap.lookup rate brackets
        successorRate <- rateSuccessor obs rate
        successorThreshold <- NEMap.lookup successorRate brackets
        Just $ thresholdDifference threshold successorThreshold
    )

taxToEndOfOrdinaryBracket :: OrdinaryBrackets -> FederalTaxRate -> TaxPayable
taxToEndOfOrdinaryBracket brackets bracketRate =
  let relevantRates = List.takeWhile (<= bracketRate) (ordinaryRatesExceptTop brackets)
      bracketWidths = List.map (ordinaryIncomeBracketWidth brackets) relevantRates
      pairs = relevantRates `zip` bracketWidths
      taxesDue = List.map taxForBracket pairs
        where
          taxForBracket :: (FederalTaxRate, TaxableIncome) -> TaxPayable
          taxForBracket (rate, width) = applyTaxRate rate width
   in mconcat taxesDue
