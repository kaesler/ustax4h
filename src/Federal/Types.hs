module Federal.Types
  ( CombinedIncome,
    DistributionPeriod,
    ItemizedDeductions,
    OrdinaryIncome,
    PersonalExemptions,
    QualifiedIncome,
    SocSec,
    SSRelevantOtherIncome,
    StandardDeduction,
  )
where

import Moneys

type PersonalExemptions = Int

type OrdinaryIncome = Income

type QualifiedIncome = Income

type SSRelevantOtherIncome = Income

type SocSec = Income

type CombinedIncome = Income

type DistributionPeriod = Double

type StandardDeduction = Deduction

type ItemizedDeductions = Deduction
