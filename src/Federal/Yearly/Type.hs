module Federal.Yearly.Type
  ( YearlyValues (..),
  )
where

import CommonTypes (FilingStatus, Year)
import Federal.OrdinaryBrackets (OrdinaryBrackets)
import Federal.QualifiedBrackets (QualifiedBrackets)
import Federal.Regime (Regime)
import Moneys (Deduction)

data YearlyValues = YearlyValues
  { regime :: Regime,
    year :: Year,
    perPersonExemption :: Deduction,
    unadjustedStandardDeduction :: FilingStatus -> Deduction,
    adjustmentWhenOver65 :: Deduction,
    adjustmentWhenOver65AndSingle :: Deduction,
    ordinaryBrackets :: FilingStatus -> OrdinaryBrackets,
    qualifiedBrackets :: FilingStatus -> QualifiedBrackets
  }
