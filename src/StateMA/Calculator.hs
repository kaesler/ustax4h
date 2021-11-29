module StateMA.Calculator
  ( taxDue,
  )
where

import CommonTypes
  ( FilingStatus (..),
    MassachusettsGrossIncome,
    Money,
    Year,
  )
import Math (nonNegSub)

taxRate :: Double
taxRate = 0.05

personalExemptionFor :: Year -> FilingStatus -> Money 
personalExemptionFor _ HeadOfHousehold = 6800
personalExemptionFor _ Single = 4400

taxDue :: Year -> Int -> FilingStatus -> MassachusettsGrossIncome -> Money
taxDue year dependents filingStatus maGrossIncome =
  let personalExemption = personalExemptionFor year filingStatus
      ageExemption = 700
      dependentsExemption = 1000.0 * fromIntegral dependents
   in taxRate * (maGrossIncome `nonNegSub` (personalExemption + ageExemption + dependentsExemption))
