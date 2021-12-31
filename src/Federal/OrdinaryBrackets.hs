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
import Federal.FederalTaxRate (FederalTaxRate, mkFederalTaxRate)
import Money.Money (IncomeThreshold, TaxPayable, TaxableIncome)

newtype OrdinaryBrackets = OrdinaryBrackets (Brackets.Brackets FederalTaxRate)

-- TODO: do we need all these?

inflateThresholds :: Double -> OrdinaryBrackets -> OrdinaryBrackets
inflateThresholds factor (OrdinaryBrackets brackets) = coerce $ Brackets.inflateThresholds factor brackets

fromPairs :: [(Double, Integer)] -> OrdinaryBrackets
fromPairs pairs = coerce $ Brackets.fromPairs pairs mkFederalTaxRate

rateSuccessor :: FederalTaxRate -> OrdinaryBrackets -> Maybe FederalTaxRate
rateSuccessor rate brackets = coerce $ Brackets.rateSuccessor rate (coerce brackets)

ordinaryRatesExceptTop :: OrdinaryBrackets -> [FederalTaxRate]
ordinaryRatesExceptTop brackets = coerce $ Brackets.ratesExceptTop (coerce brackets)

taxableIncomeToEndOfOrdinaryBracket :: OrdinaryBrackets -> FederalTaxRate -> IncomeThreshold
taxableIncomeToEndOfOrdinaryBracket brackets = Brackets.taxableIncomeToEndOfBracket (coerce brackets)

ordinaryIncomeBracketWidth :: OrdinaryBrackets -> FederalTaxRate -> TaxableIncome
ordinaryIncomeBracketWidth brackets = Brackets.bracketWidth (coerce brackets)

taxToEndOfOrdinaryBracket :: OrdinaryBrackets -> FederalTaxRate -> TaxPayable
taxToEndOfOrdinaryBracket brackets = Brackets.taxToEndOfBracket (coerce brackets)
