module Federal.Yearly.Type(
  YearlyValues (..)
) where

import Federal.Regime (Regime)
import CommonTypes (Year, FilingStatus)
import Moneys (Deduction)
import Federal.OrdinaryBrackets (OrdinaryBrackets)
import Federal.QualifiedBrackets (QualifiedBrackets)

data YearlyValues = YearlyValues
  { 
    regime :: Regime,
    year :: Year,
    perPersonExemption :: Deduction,
    unadjustedStandardDeduction :: FilingStatus -> Deduction,
    adjustmentWhenOver65 :: Deduction,
    adjustmentWhenOver65AndSingle :: Deduction,
    ordinaryBrackets :: FilingStatus -> OrdinaryBrackets,
    qualifiedBrackets :: FilingStatus -> QualifiedBrackets
  }
