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
import StateMA.StateMATaxRate (StateMATaxRate, mkStateMATaxRate)
import Money.Money (Deduction, mkDeduction, applyDeductions, Income, TaxPayable)
import qualified TaxFunction

taxRate :: Year -> StateMATaxRate  
taxRate year
  | fromEnum year == 2020 = mkStateMATaxRate 0.05
  | fromEnum year == 2019 = mkStateMATaxRate 0.0505
  | fromEnum year == 2018 = mkStateMATaxRate 0.051
  | fromEnum year < 2018 = mkStateMATaxRate 0.051
  | otherwise = mkStateMATaxRate 0.05

taxFunction  :: Year -> TaxFunction.TaxFunction 
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
   in 
     taxFunction year taxableIncome

