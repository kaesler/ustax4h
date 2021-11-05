module CommonTypes
  ( Age (..),
    AnnualGrowthRatePercentage,
    BirthDate,
    CombinedIncome,
    DistributionPeriod,
    FilingStatus (..),
    InflationEstimate (..),
    inflationFactor,
    ItemizedDeductions,
    MassachusettsGrossIncome,
    Money,
    OrdinaryIncome,
    PersonalExemptions,
    QualifiedIncome,
    SSRelevantOtherIncome,
    SocSec,
    StandardDeduction (..),
    Year,
  )
where

import Data.Time.Calendar (Day)
import GHC.Float (powerDouble)

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

-- TODO: just Integer?
newtype StandardDeduction = StandardDeduction Integer
  deriving (Eq, Ord, Show)

type AnnualGrowthRatePercentage = Double

-- target year, growth rate as a percentage
data InflationEstimate = InflationEstimate Year AnnualGrowthRatePercentage

inflationFactor :: InflationEstimate -> Year -> Double
inflationFactor (InflationEstimate targetYear annualGrowthRate) baseYear
  | targetYear <= baseYear = error "Inflation goes forward"
  | otherwise = powerDouble (1 + annualGrowthRate) $ fromIntegral (targetYear - baseYear)