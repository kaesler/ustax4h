module CommonTypes
  ( Age (..),
    CombinedIncome,
    DistributionPeriod,
    FilingStatus (..),
    MassachusettsGrossIncome,
    OrdinaryIncome,
    QualifiedIncome,
    SSRelevantOtherIncome,
    SocSec,
    Year,
  )
where

type CombinedIncome = Double

type DistributionPeriod = Double

type MassachusettsGrossIncome = Double

type OrdinaryIncome = Double

type QualifiedIncome = Double

type SSRelevantOtherIncome = Double

type SocSec = Double

type Year = Integer

newtype Age = Age Integer
  deriving (Eq, Ord, Show)

data FilingStatus = HeadOfHousehold | Single
  deriving (Eq, Ord, Show, Enum)
