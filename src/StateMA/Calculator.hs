module StateMA.Calculator
  ( taxDue,
  )
where

import Age (isAge65OrOlder)
import CommonTypes
  ( BirthDate,
    FilingStatus (..),
    Money,
    Year,
  )
import Math (nonNegSub)
import StateMA.Types (MassachusettsGrossIncome)

taxRate :: Year -> Double
taxRate year
  | fromEnum year == 2020 = 0.05
  | fromEnum year == 2019 = 0.0505
  | fromEnum year == 2018 = 0.051
  | fromEnum year < 2018 = 0.051
  | otherwise = 0.05

personalExemptionFor :: Year -> FilingStatus -> Money
personalExemptionFor _ HeadOfHousehold = 6800
personalExemptionFor _ Single = 4400

taxDue :: Year -> BirthDate -> Int -> FilingStatus -> MassachusettsGrossIncome -> Money
taxDue year birthDate dependents filingStatus maGrossIncome =
  let personalExemption = personalExemptionFor year filingStatus
      ageExemption = if isAge65OrOlder birthDate year then 700 else 0
      dependentsExemption = 1000.0 * fromIntegral dependents
   in taxRate year * (maGrossIncome `nonNegSub` (personalExemption + ageExemption + dependentsExemption))
