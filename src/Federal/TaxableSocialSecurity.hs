module Federal.TaxableSocialSecurity(
  taxableSocialSecurity
) where

import CommonTypes
    ( SocSec, SSRelevantOtherIncome, CombinedIncome, FilingStatus(..) )

taxableSocialSecurity :: FilingStatus -> SocSec -> SSRelevantOtherIncome -> Double
taxableSocialSecurity filingStatus ssBenefits relevantIncome =
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
