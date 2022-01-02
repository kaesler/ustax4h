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
import Data.Function
import Math (nonNegSub)
import Moneys (Deduction, Income, TaxPayable, applyDeductions, mkDeduction)
import StateMA.StateMATaxRate (StateMATaxRate, mkStateMATaxRate)
import StateMA.Types (MassachusettsGrossIncome)
import qualified TaxFunction

taxRate :: Year -> StateMATaxRate
taxRate year = mkStateMATaxRate $ selectRate $ fromEnum year
  where
    selectRate i
      | i == 2020 = 0.05
      | i == 2019 = 0.0505
      | i == 2018 = 0.051
      | i < 2018 = 0.051
      | otherwise = 0.05

taxFunction :: Year -> TaxFunction.TaxFunction
taxFunction = TaxFunction.flatTaxFunction . taxRate

personalExemptionFor :: Year -> FilingStatus -> Deduction
personalExemptionFor _ HeadOfHousehold = mkDeduction 6800
personalExemptionFor _ Single = mkDeduction 4400

taxDue :: Year -> BirthDate -> Int -> FilingStatus -> Income -> TaxPayable
taxDue year birthDate dependents filingStatus maGrossIncome =
  let personalExemption = personalExemptionFor year filingStatus
      ageExemption = mkDeduction (if isAge65OrOlder birthDate year then 700 else 0)
      dependentsExemption = mkDeduction $ 1000 * dependents
      deductions = mconcat [personalExemption, ageExemption, dependentsExemption]
      taxableIncome = maGrossIncome `applyDeductions` deductions
   in taxFunction year taxableIncome
