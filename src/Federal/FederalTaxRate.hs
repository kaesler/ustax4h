module Federal.FederalTaxRate(
  FederalTaxRate,
  mkFederalTaxRate
)

where

import Text.Printf (printf)
import TaxRate ( TaxRate(..) )

newtype FederalTaxRate = FederalTaxRate Double 

mkFederalTaxRate :: Double -> FederalTaxRate
mkFederalTaxRate d
  | d < 0.0 = error $ printf "Invalid FederalTaxRate %d" d
  | d > 0.9 = error  $ printf "Invalid FederalTaxRate %d" d
  | otherwise = FederalTaxRate d

instance TaxRate FederalTaxRate where
  zero = mkFederalTaxRate 0.0
  toDouble (FederalTaxRate d) = d
  absoluteDifference (FederalTaxRate d1) (FederalTaxRate d2) = 
    FederalTaxRate (abs (d1 - d2))
