{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Federal.OrdinaryBrackets
  ( OrdinaryBrackets,
    inflateThresholds,
    fromPairs,
    ordinaryIncomeBracketWidth,
    ordinaryRatesExceptTop,
    rateSuccessor,
    taxableIncomeToEndOfOrdinaryBracket,
    taxFunctionFor,
    taxToEndOfOrdinaryBracket,
  )
where

import qualified Brackets
import Data.Coerce (coerce)
import Federal.FederalTaxRate (FederalTaxRate, mkFederalTaxRate)
import Moneys (TaxPayable, TaxableIncome)
import TaxFunction (TaxFunction, bracketsTaxFunction)

newtype OrdinaryBrackets = OrdinaryBrackets (Brackets.Brackets FederalTaxRate)
  deriving newtype (Show)

fromPairs :: [(Double, Int)] -> OrdinaryBrackets
fromPairs pairs = coerce $ Brackets.fromPairs pairs mkFederalTaxRate

inflateThresholds :: Double -> OrdinaryBrackets -> OrdinaryBrackets
inflateThresholds factor (OrdinaryBrackets brackets) = coerce $ Brackets.inflateThresholds factor brackets

taxFunctionFor :: OrdinaryBrackets -> TaxFunction
taxFunctionFor (OrdinaryBrackets brs) = bracketsTaxFunction brs

rateSuccessor :: FederalTaxRate -> OrdinaryBrackets -> Maybe FederalTaxRate
rateSuccessor rate brackets = coerce $ Brackets.rateSuccessor rate (coerce brackets)

ordinaryRatesExceptTop :: OrdinaryBrackets -> [FederalTaxRate]
ordinaryRatesExceptTop brackets = coerce $ Brackets.ratesExceptTop (coerce brackets)

taxableIncomeToEndOfOrdinaryBracket :: OrdinaryBrackets -> FederalTaxRate -> TaxableIncome
taxableIncomeToEndOfOrdinaryBracket brackets = Brackets.taxableIncomeToEndOfBracket (coerce brackets)

ordinaryIncomeBracketWidth :: OrdinaryBrackets -> FederalTaxRate -> TaxableIncome
ordinaryIncomeBracketWidth brackets = Brackets.bracketWidth (coerce brackets)

taxToEndOfOrdinaryBracket :: OrdinaryBrackets -> FederalTaxRate -> TaxPayable
taxToEndOfOrdinaryBracket brackets = Brackets.taxToEndOfBracket (coerce brackets)
