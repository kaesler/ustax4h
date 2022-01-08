{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Federal.FederalTaxRate
  ( FederalTaxRate,
    mkFederalTaxRate,
  )
where

import TaxRate (TaxRate (..))
import Text.Printf (printf)

newtype FederalTaxRate = FederalTaxRate Double
  deriving newtype (Eq, Ord, Show)

mkFederalTaxRate :: Double -> FederalTaxRate
mkFederalTaxRate d
  | d < 0.0 = error $ printf "Invalid FederalTaxRate %d" d
  | d > 0.9 = error $ printf "Invalid FederalTaxRate %d" d
  | otherwise = FederalTaxRate d

instance TaxRate FederalTaxRate where
  zero = mkFederalTaxRate 0.0
  toDouble (FederalTaxRate d) = d
  absoluteDifference (FederalTaxRate d1) (FederalTaxRate d2) =
    FederalTaxRate (abs (d1 - d2))
