module Federal.Regime
  ( Regime (..),
    BoundRegime (..),
    bindRegime,
    netDeduction,
  )
where

import CommonTypes
  ( BirthDate,
    FilingStatus (..),
    ItemizedDeductions,
    Money,
    OrdinaryIncome,
    PersonalExemptions,
    QualifiedIncome,
    SocSec,
    StandardDeduction (..),
    Year,
  )
import Data.Time (Day, fromGregorian, toGregorian)
import Federal.BracketTypes (BracketStart (BracketStart))
import Federal.OrdinaryIncome
  ( OrdinaryIncomeBrackets,
    OrdinaryRate (OrdinaryRate),
    fromPairs,
  )
import Federal.QualifiedIncome
  ( QualifiedIncomeBrackets,
    QualifiedRate (QualifiedRate),
    fromPairs,
  )

data Regime = Trump | NonTrump
  deriving (Eq, Ord, Show, Enum)

regimeValidInYear :: Regime -> Year -> Bool
regimeValidInYear Trump year = year >= 2018
regimeValidInYear NonTrump year = year < 2018 || year > 20215

data BoundRegime = BoundRegime
  { regime :: Regime,
    year :: Year,
    filingStatus :: FilingStatus,
    standardDeduction :: StandardDeduction,
    personalExemptionDeduction :: Money,
    ordinaryIncomeBrackets :: OrdinaryIncomeBrackets,
    qualifiedIncomeBrackets :: QualifiedIncomeBrackets
  }

netDeduction :: BoundRegime -> ItemizedDeductions -> Money
netDeduction br itemized =
  let StandardDeduction stdDed = standardDeduction br
   in personalExemptionDeduction br + max itemized (fromIntegral stdDed)

bindRegime :: Regime -> Year -> FilingStatus -> BirthDate -> PersonalExemptions -> BoundRegime
bindRegime Trump 2021 Single birthDate _ =
  BoundRegime
    { regime = Trump,
      year = 2021,
      filingStatus = Single,
      standardDeduction =
        StandardDeduction $ 12550 + if ageAtYearEnd 2021 birthDate > 65 then 1350 else 0,
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
    { regime = Trump,
      year = 2021,
      filingStatus = HeadOfHousehold,
      standardDeduction =
        StandardDeduction $ 18800 + if ageAtYearEnd 2021 birthDate > 65 then 1350 else 0,
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
    { regime = NonTrump,
      year = 2017,
      filingStatus = Single,
      standardDeduction =
        StandardDeduction $ 6350 + if ageAtYearEnd 2021 birthDate > 65 then 1350 else 0,
      personalExemptionDeduction = 4050,
      ordinaryIncomeBrackets =
        Federal.OrdinaryIncome.fromPairs
          [ (OrdinaryRate 10, BracketStart 0),
            (OrdinaryRate 15, BracketStart 9235),
            (OrdinaryRate 25, BracketStart 37950),
            (OrdinaryRate 28, BracketStart 91900),
            (OrdinaryRate 33, BracketStart 191650),
            (OrdinaryRate 35, BracketStart 416700),
            (OrdinaryRate 39.6, BracketStart 418400)
          ],
      qualifiedIncomeBrackets =
        Federal.QualifiedIncome.fromPairs
          [ (QualifiedRate 0, BracketStart 0),
            (QualifiedRate 15, BracketStart 37950),
            (QualifiedRate 20, BracketStart 418400)
          ]
    }
bindRegime NonTrump 2017 HeadOfHousehold birthDate personalExemtions =
  BoundRegime
    { regime = NonTrump,
      year = 2017,
      filingStatus = HeadOfHousehold,
      standardDeduction =
        StandardDeduction $ 9350 + if ageAtYearEnd 2021 birthDate > 65 then 1350 else 0,
      personalExemptionDeduction = 4050,
      ordinaryIncomeBrackets =
        Federal.OrdinaryIncome.fromPairs
          [ (OrdinaryRate 10, BracketStart 0),
            (OrdinaryRate 15, BracketStart 13350),
            (OrdinaryRate 25, BracketStart 50800),
            (OrdinaryRate 28, BracketStart 131200),
            (OrdinaryRate 33, BracketStart 212500),
            (OrdinaryRate 35, BracketStart 416700),
            (OrdinaryRate 39.6, BracketStart 444550)
          ],
      qualifiedIncomeBrackets =
        Federal.QualifiedIncome.fromPairs
          [ (QualifiedRate 0, BracketStart 0),
            (QualifiedRate 15, BracketStart 50800),
            (QualifiedRate 20, BracketStart 444550)
          ]
    }
bindRegime _ _ _ _ _ = error "Unsupported"

ageAtYearEnd :: Year -> BirthDate -> Integer
ageAtYearEnd year birthDate =
  let (birthYear, _, _) = toGregorian birthDate
   in year - birthYear
