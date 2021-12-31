module Federal.OrdinaryBrackets
  ( inflateThresholds,
    fromPairs,
    ordinaryIncomeBracketWidth,
    ordinaryRatesExceptTop,
    rateSuccessor,
    taxableIncomeToEndOfOrdinaryBracket,
    taxToEndOfOrdinaryBracket,
  )
where

import qualified Brackets
import Data.Coerce (coerce)
import qualified Data.List as List
import Data.List.NonEmpty (toList)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.NonEmpty as NEMap
import Data.Maybe (fromJust)
import Federal.FederalTaxRate (FederalTaxRate, mkFederalTaxRate)
import Money.Money (IncomeThreshold, TaxPayable, TaxableIncome, applyTaxRate, thresholdDifference)

newtype OrdinaryBrackets = OrdinaryBrackets (Brackets.Brackets FederalTaxRate)

-- TODO: do we need all these?

inflateThresholds :: Double -> OrdinaryBrackets -> OrdinaryBrackets
inflateThresholds factor (OrdinaryBrackets brackets) = coerce $ Brackets.inflateThresholds factor brackets

fromPairs :: [(Double, Integer)] -> OrdinaryBrackets
fromPairs pairs = coerce $ Brackets.fromPairs pairs mkFederalTaxRate

-- TODO: delegate to Brackets module?
rateSuccessor :: OrdinaryBrackets -> FederalTaxRate -> Maybe FederalTaxRate
rateSuccessor (OrdinaryBrackets brackets) rate =
  do
    let rates = NEMap.keys brackets
    let pairs = Prelude.zip (toList rates) (NonEmpty.tail rates)
    pair <- List.find (\p -> fst p == rate) pairs
    Just (snd pair)

-- TODO: delegate to Brackets module?
ordinaryRatesExceptTop :: OrdinaryBrackets -> [FederalTaxRate]
ordinaryRatesExceptTop (OrdinaryBrackets brackets) =
  let rates = NEMap.keys brackets
      topRate = NonEmpty.last rates
   in NonEmpty.takeWhile (/= topRate) rates

-- TODO: delegate to Brackets module?
taxableIncomeToEndOfOrdinaryBracket :: OrdinaryBrackets -> FederalTaxRate -> IncomeThreshold
taxableIncomeToEndOfOrdinaryBracket os@(OrdinaryBrackets brackets) bracketRate =
  let successorRate = fromJust (rateSuccessor os bracketRate)
   in fromJust (NEMap.lookup successorRate brackets)

-- TODO: delegate to Brackets module?
ordinaryIncomeBracketWidth :: OrdinaryBrackets -> FederalTaxRate -> TaxableIncome
ordinaryIncomeBracketWidth obs@(OrdinaryBrackets brackets) rate =
  fromJust
    ( do
        threshold <- NEMap.lookup rate brackets
        successorRate <- rateSuccessor obs rate
        successorThreshold <- NEMap.lookup successorRate brackets
        Just $ thresholdDifference threshold successorThreshold
    )

-- TODO: delegate to Brackets module?
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
