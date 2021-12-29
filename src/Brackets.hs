module Brackets (
  Brackets
)

where

import TaxRate (TaxRate)
import Money.Money (IncomeThreshold)
import Data.Map.NonEmpty (NEMap)

type Brackets r = NEMap r IncomeThreshold