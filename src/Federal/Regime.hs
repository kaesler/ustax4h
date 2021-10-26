module Federal.Regime(
  RegimeKind(..),
  BoundRegime(..)
)
where

import CommonTypes
import Federal.OrdinaryIncome ( OrdinaryIncomeBrackets )
import Federal.QualifiedIncome ( QualifiedIncomeBrackets )

data RegimeKind = Trump | NonTrump

-- bound: Year, FilingStatus, BirthDate, PersonalExemptions
data BoundRegime = BoundRegime {
  standardDeduction :: Money,
  personalExemptionDeduction :: Money,
  ordinaryIncomeBrackets :: OrdinaryIncomeBrackets,
  qualifiedIncomeBrackets :: QualifiedIncomeBrackets
}

netDeduction :: BoundRegime -> ItemizedDeductions -> Money
netDeduction br itemized = personalExemptionDeduction br + max itemized (standardDeduction br)

bindRegime :: RegimeKind -> Year -> FilingStatus -> BirthDate -> PersonalExemptions -> BoundRegime
bindRegime = undefined
