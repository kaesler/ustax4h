module CommonTypes
  ( Age (..),
    AnnualGrowthRatePercentage,
    BirthDate,
    FilingStatus (..),
    InflationEstimate (..),
    inflationFactor,
    isUnmarried,
    Year,
  )
where

import Data.Time.Calendar (Day)
import GHC.Float (powerDouble)

type BirthDate = Day

type Year = Integer

newtype Age = Age Int
  deriving (Eq, Ord, Show)

data FilingStatus = Married | HeadOfHousehold | Single
  deriving (Eq, Ord, Show, Enum)

isUnmarried :: FilingStatus -> Bool
isUnmarried Married = False
isUnmarried _ = True

type AnnualGrowthRatePercentage = Double

-- target year, growth rate as a percentage
data InflationEstimate = InflationEstimate Year AnnualGrowthRatePercentage

inflationFactor :: InflationEstimate -> Year -> Double
inflationFactor (InflationEstimate targetYear annualGrowthRate) baseYear
  | targetYear <= baseYear = error "Inflation goes forward"
  | otherwise = powerDouble (1 + annualGrowthRate) $ fromIntegral (targetYear - baseYear)
