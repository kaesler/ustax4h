{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Federal.QualifiedBrackets
  ( QualifiedBrackets,
    inflateThresholds,
    fromPairs,
    taxFunctionFor,
  )
where

import qualified Brackets
import Data.Coerce (coerce)
import Federal.FederalTaxRate (FederalTaxRate, mkFederalTaxRate)
import TaxFunction (TaxFunction, bracketsTaxFunction)

newtype QualifiedBrackets = QualifiedBrackets (Brackets.Brackets FederalTaxRate)
  deriving newtype (Show)

fromPairs :: [(Int, Double)] -> QualifiedBrackets
fromPairs pairs = coerce $ Brackets.fromPairs pairs mkFederalTaxRate

taxFunctionFor :: QualifiedBrackets -> TaxFunction
taxFunctionFor (QualifiedBrackets brs) = bracketsTaxFunction brs

inflateThresholds :: Double -> QualifiedBrackets -> QualifiedBrackets
inflateThresholds factor (QualifiedBrackets brackets) = coerce $ Brackets.inflateThresholds factor brackets
