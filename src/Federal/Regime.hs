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
    InflationEstimate (..),
    ItemizedDeductions,
    Money,
    OrdinaryIncome,
    PersonalExemptions,
    QualifiedIncome,
    SocSec,
    StandardDeduction (..),
    Year,
    inflationFactor,
  )
import Data.Time (Day, UniversalTime, fromGregorian, toGregorian)
import Federal.BracketTypes (BracketStart (BracketStart))
import qualified Federal.OrdinaryIncome as FO
  ( OrdinaryIncomeBrackets,
    OrdinaryRate (OrdinaryRate),
    fromPairs,
    fromPairsX,
    inflate,
  )
import qualified Federal.QualifiedIncome as FQ
  ( QualifiedIncomeBrackets,
    QualifiedRate (QualifiedRate),
    fromPairs,
    fromPairsX,
    inflate,
  )
import GHC.Stack (HasCallStack)
import Text.Printf

data Regime = Trump | NonTrump
  deriving (Eq, Ord, Show, Enum)

requireRegimeValidInYear :: Regime -> Year -> ()
requireRegimeValidInYear regime year =
  if regimeValidInYear regime year
    then ()
    else error $ printf "Regime %s not valid in year %d" (show regime) year

regimeValidInYear :: Regime -> Year -> Bool
regimeValidInYear Trump year = year >= 2018
regimeValidInYear NonTrump year = year < 2018 || year > 20215

data BoundRegime = BoundRegime
  { regime :: Regime,
    year :: Year,
    filingStatus :: FilingStatus,
    standardDeduction :: StandardDeduction,
    personalExemptionDeduction :: Money,
    ordinaryIncomeBrackets :: FO.OrdinaryIncomeBrackets,
    qualifiedIncomeBrackets :: FQ.QualifiedIncomeBrackets
  }

netDeduction :: BoundRegime -> ItemizedDeductions -> Money
netDeduction br itemized =
  let StandardDeduction stdDed = standardDeduction br
   in personalExemptionDeduction br + max itemized (fromIntegral stdDed)

bindRegime :: HasCallStack => Regime -> Year -> FilingStatus -> BirthDate -> PersonalExemptions -> BoundRegime
bindRegime Trump 2021 Single birthDate _ =
  BoundRegime
    { regime = Trump,
      year = 2021,
      filingStatus = Single,
      standardDeduction =
        StandardDeduction $ 12550 + if ageAtYearEnd 2021 birthDate > 65 then 1350 else 0,
      personalExemptionDeduction = 0,
      ordinaryIncomeBrackets =
        FO.fromPairsX
          [ (10, 0),
            (12, 9950),
            (22, 40525),
            (24, 86375),
            (32, 164925),
            (35, 209425),
            (37, 523600)
          ],
      qualifiedIncomeBrackets =
        FQ.fromPairsX
          [ (0, 0),
            (15, 40400),
            (20, 445850)
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
        FO.fromPairsX
          [ (10, 0),
            (12, 14200),
            (22, 54200),
            (24, 86350),
            (32, 164900),
            (35, 209400),
            (37, 523600)
          ],
      qualifiedIncomeBrackets =
        FQ.fromPairsX
          [ (0, 0),
            (15, 54100),
            (20, 473850)
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
        FO.fromPairsX
          [ (10, 0),
            (15, 9235),
            (25, 37950),
            (28, 91900),
            (33, 191650),
            (35, 416700),
            (39.6, 418400)
          ],
      qualifiedIncomeBrackets =
        FQ.fromPairsX
          [ (0, 0),
            (15, 37950),
            (20, 418400)
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
        FO.fromPairsX
          [ (10, 0),
            (15, 13350),
            (25, 50800),
            (28, 131200),
            (33, 212500),
            (35, 416700),
            (39.6, 444550)
          ],
      qualifiedIncomeBrackets =
        FQ.fromPairsX
          [ (0, 0),
            (15, 50800),
            (20, 444550)
          ]
    }
bindRegime regime year filingStatus _ _ =
  error $ printf "Unsupported combination %s, %d, %s " (show regime) year (show filingStatus)

futureEstimated :: BoundRegime -> InflationEstimate -> BoundRegime
futureEstimated br inflationEstimate =
  let factor = inflationFactor inflationEstimate (year br)
      InflationEstimate futureYear _ = inflationEstimate
      StandardDeduction oldStdDed = standardDeduction br
   in br
        { year = futureYear,
          standardDeduction = StandardDeduction $ round $ factor * fromIntegral oldStdDed,
          personalExemptionDeduction = personalExemptionDeduction br * factor,
          ordinaryIncomeBrackets = FO.inflate (ordinaryIncomeBrackets br) factor,
          qualifiedIncomeBrackets = FQ.inflate (qualifiedIncomeBrackets br) factor
        }

ageAtYearEnd :: Year -> BirthDate -> Integer
ageAtYearEnd year birthDate =
  let (birthYear, _, _) = toGregorian birthDate
   in year - birthYear
