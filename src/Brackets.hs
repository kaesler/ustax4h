module Brackets (
  Brackets
)

where

import TaxRate (TaxRate)
import Money.IncomeThreshold (IncomeThreshold)
import Data.Map.NonEmpty (NEMap)

type Brackets r = NEMap IncomeThreshold r