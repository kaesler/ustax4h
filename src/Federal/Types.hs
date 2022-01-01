module Federal.Types
  ( CombinedIncome,
    DistributionPeriod,
    ItemizedDeductions,
    PersonalExemptions,
    OrdinaryIncome,
    QualifiedIncome,
    SSRelevantOtherIncome,
    SocSec,
    StandardDeduction (..),
  )
where

import CommonTypes (Money)

type ItemizedDeductions = Money

type PersonalExemptions = Int

type OrdinaryIncome = Money

type QualifiedIncome = Money

type SSRelevantOtherIncome = Money

type SocSec = Money

type CombinedIncome = Money

type DistributionPeriod = Double

newtype StandardDeduction = StandardDeduction Int
  deriving (Eq, Ord, Show)
