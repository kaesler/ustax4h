module Federal.Deductions
  ( StandardDeduction (..),
    standardDeductionFor,
  )
where

import CommonTypes

newtype StandardDeduction = StandardDeduction Integer
  deriving (Eq, Ord, Show)

over65Increment :: Integer
over65Increment = 1350

-- TODO: remove
standardDeductionFor :: Year -> FilingStatus -> StandardDeduction
standardDeductionFor _ HeadOfHousehold = StandardDeduction (18800 + over65Increment)
standardDeductionFor _ Single = StandardDeduction (12550 + over65Increment)
