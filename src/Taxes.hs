module Taxes
  ( Age (..),
    FilingStatus (..),
    OrdinaryRate (..),
    QualifiedRate (..),
    StandardDeduction (..),
    bracketWidth
  )
where

import qualified Data.Map.Strict as Map
import qualified Data.List as List
import Data.Coerce ( coerce )

newtype Age = Age Int
  deriving (Eq, Ord, Show)

data FilingStatus = HeadOfHousehold | Single
  deriving (Eq, Ord, Show)

newtype OrdinaryRate = OrdinaryRate Int
  deriving (Eq, Ord, Show)

newtype QualifiedRate = QualifiedRate Int
  deriving (Eq, Ord, Show)

newtype BracketStart = BracketStart Int
  deriving (Eq, Ord, Show)

newtype StandardDeduction = StandardDeduction Int
  deriving (Eq, Ord, Show)

type OrdinaryBracketStarts = Map.Map OrdinaryRate BracketStart

type QualifiedBracketStarts = Map.Map QualifiedRate BracketStart

ordinaryBracketStarts :: Map.Map FilingStatus (Map.Map OrdinaryRate BracketStart)
ordinaryBracketStarts =
  Map.fromList
    [ ( Single,
        Map.fromList
          [ (OrdinaryRate 10, BracketStart 0),
            (OrdinaryRate 12, BracketStart 9950),
            (OrdinaryRate 22, BracketStart 40525),
            (OrdinaryRate 24, BracketStart 86375),
            (OrdinaryRate 32, BracketStart 164925),
            (OrdinaryRate 35, BracketStart 209425),
            (OrdinaryRate 37, BracketStart 523600)
          ]
      ),
      ( HeadOfHousehold,
        Map.fromList
          [ (OrdinaryRate 10, BracketStart 0),
            (OrdinaryRate 12, BracketStart 14200),
            (OrdinaryRate 22, BracketStart 54200),
            (OrdinaryRate 24, BracketStart 86350),
            (OrdinaryRate 32, BracketStart 164900),
            (OrdinaryRate 35, BracketStart 209400),
            (OrdinaryRate 37, BracketStart 523600)
          ]
      )
    ]

qualifiedBracketStarts :: Map.Map FilingStatus (Map.Map QualifiedRate BracketStart)
qualifiedBracketStarts =
  Map.fromList
    [ ( Single,
        Map.fromList
          [ (QualifiedRate 0, BracketStart 0),
            (QualifiedRate 15, BracketStart 40400),
            (QualifiedRate 20, BracketStart 445850)
          ]
      ),
      ( HeadOfHousehold,
        Map.fromList
          [ (QualifiedRate 0, BracketStart 0),
            (QualifiedRate 15, BracketStart 54100),
            (QualifiedRate 20, BracketStart 473850)
          ]
      )
    ]

newtype DistributionPeriod = DistributionPeriod Float

distributionPeriods :: Map.Map Age DistributionPeriod
distributionPeriods =
  Map.fromList
    [ (Age 70, DistributionPeriod 27.4),
      (Age 71, DistributionPeriod 26.5),
      (Age 72, DistributionPeriod 25.6),
      (Age 73, DistributionPeriod 24.7),
      (Age 74, DistributionPeriod 23.8),
      (Age 75, DistributionPeriod 22.9),
      (Age 76, DistributionPeriod 22.0),
      (Age 77, DistributionPeriod 21.2),
      (Age 78, DistributionPeriod 20.3),
      (Age 79, DistributionPeriod 19.5),
      (Age 80, DistributionPeriod 18.7),
      (Age 81, DistributionPeriod 17.9),
      (Age 82, DistributionPeriod 17.1),
      (Age 83, DistributionPeriod 16.3),
      (Age 84, DistributionPeriod 15.5),
      (Age 85, DistributionPeriod 14.8),
      (Age 86, DistributionPeriod 14.1),
      (Age 87, DistributionPeriod 13.4),
      (Age 88, DistributionPeriod 12.7),
      (Age 89, DistributionPeriod 12.0),
      (Age 90, DistributionPeriod 11.4),
      (Age 91, DistributionPeriod 10.8),
      (Age 92, DistributionPeriod 10.2),
      (Age 93, DistributionPeriod 9.6),
      (Age 94, DistributionPeriod 9.1),
      (Age 95, DistributionPeriod 8.6),
      (Age 96, DistributionPeriod 8.1),
      (Age 97, DistributionPeriod 7.6),
      (Age 98, DistributionPeriod 7.1),
      (Age 99, DistributionPeriod 6.7),
      (Age 100, DistributionPeriod 6.3),
      (Age 101, DistributionPeriod 5.9),
      (Age 102, DistributionPeriod 5.5),
      (Age 103, DistributionPeriod 5.2),
      (Age 104, DistributionPeriod 4.9),
      (Age 105, DistributionPeriod 4.5),
      (Age 106, DistributionPeriod 4.2),
      (Age 107, DistributionPeriod 3.9),
      (Age 108, DistributionPeriod 3.7),
      (Age 109, DistributionPeriod 3.4),
      (Age 110, DistributionPeriod 3.1),
      (Age 111, DistributionPeriod 2.9),
      (Age 112, DistributionPeriod 2.6),
      (Age 113, DistributionPeriod 2.4),
      (Age 114, DistributionPeriod 2.1)
    ]

over65Increment :: Int
over65Increment = 1350

standardDeduction :: FilingStatus -> StandardDeduction
standardDeduction HeadOfHousehold = StandardDeduction (18800 + over65Increment)
standardDeduction Single = StandardDeduction (12550 + over65Increment)

{- ordinaryBracketWidth :: FilingStatus -> OrdinaryRate -> Int -}

fail :: () -> a
fail = error "boom"

bracketWidth :: FilingStatus -> OrdinaryRate -> Maybe Int
bracketWidth fs rate = 
  do
    brackets <- Map.lookup fs ordinaryBracketStarts
    let rates = Map.keys brackets
    let pairs = zip rates (tail rates) 
    pair <- List.find (\p -> fst p == rate) pairs
    let successor = snd pair
    rateStart <- Map.lookup rate brackets
    successorStart <- Map.lookup successor brackets
    Just (coerce successorStart - coerce rateStart)
