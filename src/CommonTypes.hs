module CommonTypes
  ( Age (..),
    BirthDate,
    CombinedIncome,
    DistributionPeriod,
    FilingStatus (..),
    ItemizedDeductions,
    MassachusettsGrossIncome,
    Money,
    OrdinaryIncome,
    PersonalExemptions,
    QualifiedIncome,
    SSRelevantOtherIncome,
    SocSec,
    Year,
  )

where

import Data.Time.Calendar (Day)

type BirthDate = Day

type Money = Double

type ItemizedDeductions = Money

type CombinedIncome = Money

type DistributionPeriod = Double

type MassachusettsGrossIncome = Money

type OrdinaryIncome = Money

type PersonalExemptions = Int 

type QualifiedIncome = Money

type SSRelevantOtherIncome = Money

type SocSec = Money

type Year = Integer

newtype Age = Age Integer
  deriving (Eq, Ord, Show)

data FilingStatus = HeadOfHousehold | Single
  deriving (Eq, Ord, Show, Enum)

type AnnualGrowthRatePercentage = Double

-- target year, growth rate as a percentage
data Inflation = Inflation Year AnnualGrowthRatePercentage

