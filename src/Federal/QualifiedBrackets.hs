module Federal.QualifiedBrackets
  ( inflateThresholds,
    fromPairs,
  )
where

import qualified Brackets
import Data.Coerce (coerce)
import Federal.FederalTaxRate (FederalTaxRate, mkFederalTaxRate)

newtype QualifiedBrackets = QualifiedBrackets (Brackets.Brackets FederalTaxRate)

inflateThresholds :: Double -> QualifiedBrackets -> QualifiedBrackets
inflateThresholds factor (QualifiedBrackets brackets) = coerce $ Brackets.inflateThresholds factor brackets

fromPairs :: [(Double, Integer)] -> QualifiedBrackets
fromPairs pairs = coerce $ Brackets.fromPairs pairs mkFederalTaxRate
