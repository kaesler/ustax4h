module MAStateTax
  ( maStateTaxDue,
  )
where

import CommonTypes
    ( Year, MassachusettsGrossIncome, FilingStatus(HeadOfHousehold) )
import Math ( nonNegSub )

maStateTaxRate :: Double
maStateTaxRate = 0.05

maStateTaxDue :: Year -> Int -> FilingStatus -> MassachusettsGrossIncome -> Double
maStateTaxDue year dependents filingStatus maGrossIncome =
  let personalExemption = if filingStatus == HeadOfHousehold then 6800 else 4400
      ageExemption = 700
      dependentsExemption = 1000.0 * fromIntegral dependents
   in maStateTaxRate * (maGrossIncome `nonNegSub` (personalExemption + ageExemption + dependentsExemption))
