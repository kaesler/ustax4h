module Federal.Regime
  ( RegimeKind (..),
    BoundRegime (..),
  )
where

import CommonTypes
    ( BirthDate,
      FilingStatus(..),
      ItemizedDeductions,
      Money,
      PersonalExemptions,
      Year )
import Federal.BracketTypes (BracketStart (BracketStart))
import Federal.OrdinaryIncome (OrdinaryIncomeBrackets, OrdinaryRate (..), fromPairs)
import Federal.QualifiedIncome (QualifiedIncomeBrackets, QualifiedRate (..), fromPairs)
import Data.Time (toGregorian)

data RegimeKind = Trump | NonTrump

-- bound: Year, FilingStatus, BirthDate, PersonalExemptions
data BoundRegime = BoundRegime
  { standardDeduction :: Money,
    personalExemptionDeduction :: Money,
    ordinaryIncomeBrackets :: OrdinaryIncomeBrackets,
    qualifiedIncomeBrackets :: QualifiedIncomeBrackets
  }

netDeduction :: BoundRegime -> ItemizedDeductions -> Money
netDeduction br itemized = personalExemptionDeduction br + max itemized (standardDeduction br)

bindRegime :: RegimeKind -> Year -> FilingStatus -> BirthDate -> PersonalExemptions -> BoundRegime
bindRegime Trump 2021 Single birthDate _ =
  BoundRegime
    { standardDeduction =
        -- TODO: how to compute age at year end?
        undefined,
      personalExemptionDeduction = 0,
      ordinaryIncomeBrackets =
        Federal.OrdinaryIncome.fromPairs
          [ (OrdinaryRate 10, BracketStart 0),
            (OrdinaryRate 12, BracketStart 9950),
            (OrdinaryRate 22, BracketStart 40525),
            (OrdinaryRate 24, BracketStart 86375),
            (OrdinaryRate 32, BracketStart 164925),
            (OrdinaryRate 35, BracketStart 209425),
            (OrdinaryRate 37, BracketStart 523600)
          ],
      qualifiedIncomeBrackets =
        Federal.QualifiedIncome.fromPairs
          [ (QualifiedRate 0, BracketStart 0),
            (QualifiedRate 15, BracketStart 40400),
            (QualifiedRate 20, BracketStart 445850)
          ]
    }
bindRegime Trump 2021 HeadOfHousehold birthDate _ =
  BoundRegime
    { standardDeduction = undefined,
      personalExemptionDeduction = 0,
      ordinaryIncomeBrackets =
        Federal.OrdinaryIncome.fromPairs
          [ (OrdinaryRate 10, BracketStart 0),
            (OrdinaryRate 12, BracketStart 14200),
            (OrdinaryRate 22, BracketStart 54200),
            (OrdinaryRate 24, BracketStart 86350),
            (OrdinaryRate 32, BracketStart 164900),
            (OrdinaryRate 35, BracketStart 209400),
            (OrdinaryRate 37, BracketStart 523600)
          ],
      qualifiedIncomeBrackets =
        Federal.QualifiedIncome.fromPairs
          [ (QualifiedRate 0, BracketStart 0),
            (QualifiedRate 15, BracketStart 54100),
            (QualifiedRate 20, BracketStart 473850)
          ]
    }
bindRegime NonTrump 2017 Single birthDate personalExemtions =
  BoundRegime
    { standardDeduction = 
        undefined,
      personalExemptionDeduction = undefined,
      ordinaryIncomeBrackets = undefined,
      qualifiedIncomeBrackets = undefined
    }
bindRegime NonTrump 2017 HeadOfHousehold birthDate personalExemtions =
  BoundRegime
    { standardDeduction =
        undefined,
      personalExemptionDeduction = undefined,
      ordinaryIncomeBrackets = undefined,
      qualifiedIncomeBrackets = undefined
    }
bindRegime _ _ _ _ _ = error "Unsupported"

ageAtYearEnd :: Year -> BirthDate -> Integer
ageAtYearEnd year birthDate =
  let (birthYear, _, _) = toGregorian birthDate 
  in
    year - birthYear
