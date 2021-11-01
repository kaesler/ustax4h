module StateMA.Calculator
  ( taxDue,
  )
where

import CommonTypes
  ( FilingStatus (HeadOfHousehold),
    MassachusettsGrossIncome,
    Year,
  )
import Math (nonNegSub)

taxRate :: Double
taxRate = 0.05

taxDue :: Year -> Int -> FilingStatus -> MassachusettsGrossIncome -> Double
taxDue year dependents filingStatus maGrossIncome =
  let personalExemption = if filingStatus == HeadOfHousehold then 6800 else 4400
      ageExemption = 700
      dependentsExemption = 1000.0 * fromIntegral dependents
   in taxRate * (maGrossIncome `nonNegSub` (personalExemption + ageExemption + dependentsExemption))
