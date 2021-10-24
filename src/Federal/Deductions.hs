module Federal.Deductions
  ( StandardDeduction (..),
    standardDeduction,
  )
where

import CommonTypes

newtype StandardDeduction = StandardDeduction Integer
  deriving (Eq, Ord, Show)

over65Increment :: Integer
over65Increment = 1350

standardDeduction :: Year -> FilingStatus -> StandardDeduction
standardDeduction _ HeadOfHousehold = StandardDeduction (18800 + over65Increment)
standardDeduction _ Single = StandardDeduction (12550 + over65Increment)
