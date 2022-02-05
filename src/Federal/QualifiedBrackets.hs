{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Federal.QualifiedBrackets
  ( QualifiedBrackets,
    inflateThresholds,
    fromPairs,
    fromRPairs,
    taxFunctionFor,
  )
where

import qualified Brackets
import Data.Coerce (coerce)
import Federal.FederalTaxRate (FederalTaxRate, mkFederalTaxRate)
import TaxFunction (TaxFunction, bracketsTaxFunction)

newtype QualifiedBrackets = QualifiedBrackets (Brackets.Brackets FederalTaxRate)
  deriving newtype (Show)

-- TODO get rind of one of these

fromPairs :: [(Double, Int)] -> QualifiedBrackets
fromPairs pairs = coerce $ Brackets.fromPairs pairs mkFederalTaxRate
fromRPairs :: [(Int, Double)] -> QualifiedBrackets
fromRPairs pairs = coerce $ Brackets.fromRPairs pairs mkFederalTaxRate

taxFunctionFor :: QualifiedBrackets -> TaxFunction
taxFunctionFor (QualifiedBrackets brs) = bracketsTaxFunction brs

inflateThresholds :: Double -> QualifiedBrackets -> QualifiedBrackets
inflateThresholds factor (QualifiedBrackets brackets) = coerce $ Brackets.inflateThresholds factor brackets
