module CommonTypes
  ( Age (..),
    AnnualGrowthRatePercentage,
    BirthDate,
    FilingStatus (..),
    InflationEstimate (..),
    inflationFactor,
    isUnmarried,
    Money,
    Year,
  )
where

import Data.Time.Calendar (Day)
import GHC.Float (powerDouble)

type BirthDate = Day

type Money = Double

type Year = Integer

newtype Age = Age Int
  deriving (Eq, Ord, Show)

data FilingStatus = HeadOfHousehold | Single
  deriving (Eq, Ord, Show, Enum)

isUnmarried :: FilingStatus -> Bool
isUnmarried _ = True

-- TODO: just Integer?
type AnnualGrowthRatePercentage = Double

-- target year, growth rate as a percentage
data InflationEstimate = InflationEstimate Year AnnualGrowthRatePercentage

inflationFactor :: InflationEstimate -> Year -> Double
inflationFactor (InflationEstimate targetYear annualGrowthRate) baseYear
  | targetYear <= baseYear = error "Inflation goes forward"
  | otherwise = powerDouble (1 + annualGrowthRate) $ fromIntegral (targetYear - baseYear)
