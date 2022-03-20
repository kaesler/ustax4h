module Federal.TaxableSocialSecurity
  ( amountTaxable,
    amountTaxableInflationAdjusted,
  )
where

import CommonTypes
  ( FilingStatus (..),
    Year,
  )
import Data.Function ((&))
import Federal.Types (CombinedIncome, SSRelevantOtherIncome, SocSec)
import Moneys
  ( Income,
    IncomeThreshold,
    amountOverThreshold,
    isBelow,
    makeFromInt,
    mul, thresholdDifference, divInt, taxableAsIncome
  )

amountTaxableInflationAdjusted :: Year -> FilingStatus -> SocSec -> SSRelevantOtherIncome -> Income
amountTaxableInflationAdjusted year filingStatus ssBenefits relevantIncome =
  let unadjusted = amountTaxable filingStatus ssBenefits relevantIncome
      baseYear = 2021
      annualInflationRate = 0.03
      adjustmentFactor = 1.0 + (annualInflationRate * fromIntegral (year - baseYear))
      adjusted = unadjusted `mul` adjustmentFactor
   in min adjusted (ssBenefits `mul` 0.85)

amountTaxable :: FilingStatus -> SocSec -> SSRelevantOtherIncome -> Income
amountTaxable filingStatus ssBenefits relevantIncome =
  let lowThreshold =
        ( case filingStatus of
            Married -> 32000
            HeadOfHousehold -> 25000
            Single -> 25000
        )
          & makeFromInt
      highThreshold =
        ( case filingStatus of
            Married -> 44000
            HeadOfHousehold -> 34000
            Single -> 34000
        )
          & makeFromInt
      combinedIncome = relevantIncome <> (ssBenefits `mul` 0.5)
   in f combinedIncome (lowThreshold, highThreshold)
  where
    f :: CombinedIncome -> (IncomeThreshold, IncomeThreshold) -> Income
    f combinedIncome (lowThreshold, highThreshold)
      | combinedIncome `isBelow` lowThreshold = makeFromInt 0
      | combinedIncome `isBelow` highThreshold =
        let fractionTaxable = 0.5
            maxSocSecTaxable = ssBenefits `mul` fractionTaxable
         in min ((combinedIncome `amountOverThreshold` lowThreshold) `mul` fractionTaxable) maxSocSecTaxable
      | otherwise =
        let fractionTaxable = 0.85
            maxSocSecTaxable = ssBenefits `mul` fractionTaxable
            halfMiddleBracketWidth = (highThreshold `thresholdDifference` lowThreshold) `divInt` 2
         in min
              (taxableAsIncome halfMiddleBracketWidth <> ((combinedIncome `amountOverThreshold` highThreshold) `mul` fractionTaxable))
              maxSocSecTaxable
