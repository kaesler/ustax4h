{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Federal.QualifiedBrackets
  ( QualifiedBrackets,
    inflateThresholds,
    fromPairs,
    taxFunctionFor,
    toPairs
  )
where

import qualified Brackets
import Data.Coerce (coerce)
import Federal.FederalTaxRate (FederalTaxRate, mkFederalTaxRate)
import Moneys (IncomeThreshold)
import TaxFunction (TaxFunction, bracketsTaxFunction)
import Data.List.NonEmpty (NonEmpty)

newtype QualifiedBrackets = QualifiedBrackets (Brackets.Brackets FederalTaxRate)
  deriving newtype (Show)

fromPairs :: [(Int, Double)] -> QualifiedBrackets
fromPairs pairs = coerce $ Brackets.fromPairs pairs mkFederalTaxRate

toPairs :: QualifiedBrackets -> NonEmpty (FederalTaxRate, IncomeThreshold)
toPairs (QualifiedBrackets brackets) = Brackets.toPairs brackets

taxFunctionFor :: QualifiedBrackets -> TaxFunction
taxFunctionFor (QualifiedBrackets brs) = bracketsTaxFunction brs

inflateThresholds :: Double -> QualifiedBrackets -> QualifiedBrackets
inflateThresholds factor (QualifiedBrackets brackets) = coerce $ Brackets.inflateThresholds factor brackets
