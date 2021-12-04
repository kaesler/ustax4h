module Federal.TaxableSocialSecurity
  ( amountTaxable,
    amountTaxableInflationAdjusted,
  )
where

import CommonTypes
  ( FilingStatus (..),
    Year,
  )
import Federal.Types (CombinedIncome, SSRelevantOtherIncome, SocSec)

--  TODO: use InflationEstimate type instead?
amountTaxableInflationAdjusted :: Year -> FilingStatus -> SocSec -> SSRelevantOtherIncome -> Double
amountTaxableInflationAdjusted year filingStatus ssBenefits relevantIncome =
  let unadjusted = amountTaxable filingStatus ssBenefits relevantIncome
      adjustmentFactor = 1.0 + (0.03 * fromInteger (year - 2021))
      adjusted = unadjusted * adjustmentFactor
   in min adjusted ssBenefits * 0.85

amountTaxable :: FilingStatus -> SocSec -> SSRelevantOtherIncome -> Double
amountTaxable filingStatus ssBenefits relevantIncome =
  let lowBase = case filingStatus of
        Single -> 25000
        HeadOfHousehold -> 25000
      highBase = case filingStatus of
        Single -> 34000
        HeadOfHousehold -> 34000
      combinedIncome = relevantIncome + (ssBenefits / 2.0)
   in f combinedIncome (lowBase, highBase)
  where
    f :: CombinedIncome -> (CombinedIncome, CombinedIncome) -> Double
    f combinedIncome (lowBase, highBase)
      | combinedIncome < lowBase = 0.0
      | combinedIncome < highBase =
        let fractionTaxable = 0.5
            maxSocSecTaxable = ssBenefits * fractionTaxable
         in min ((combinedIncome - lowBase) * fractionTaxable) maxSocSecTaxable
      | otherwise =
        let fractionTaxable = 0.85
            maxSocSecTaxable = ssBenefits * fractionTaxable
         in min (4500 + ((combinedIncome - highBase) * fractionTaxable)) maxSocSecTaxable
