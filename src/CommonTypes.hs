module CommonTypes
  ( Age (..),
    AnnualGrowthRatePercentage,
    BirthDate,
    FilingStatus (..),
    isUnmarried,
    Year,
  )
where

import Data.Time.Calendar (Day)

type BirthDate = Day

type Year = Integer

newtype Age = Age Int
  deriving (Eq, Ord, Show)

data FilingStatus = MarriedJoint | HeadOfHousehold | Single
  deriving (Eq, Ord, Show, Enum)

isUnmarried :: FilingStatus -> Bool
isUnmarried MarriedJoint = False
isUnmarried _ = True

type AnnualGrowthRatePercentage = Double
