module Federal.BoundRegime(
    BoundRegime (..),
    bindRegime,
    futureEstimated,
    netDeduction,
    personalExemptionDeduction,
    standardDeduction,
)
where

import CommonTypes
    ( InflationEstimate(..),
      StandardDeduction(..),
      FilingStatus(..),
      Year,
      PersonalExemptions,
      ItemizedDeductions,
      Money,
      BirthDate,
      inflationFactor )
import Federal.Regime ( Regime (NonTrump), Regime(Trump), requireRegimeValidInYear )
import Federal.OrdinaryIncome as FO ( OrdinaryIncomeBrackets, fromPairs, inflate )
import Federal.QualifiedIncome as FQ ( QualifiedIncomeBrackets, fromPairs, inflate )
import GHC.Stack (HasCallStack)
import Text.Printf ( printf )
import Data.Time (toGregorian)

data BoundRegime = BoundRegime
  { --
    -- Static field: they never change.
    regime :: Regime,
    year :: Year,
    filingStatus :: FilingStatus,
    birthDate :: BirthDate,
    personalExemptions :: Int,
    --
    -- These are inflatable. They may get adjusted to estimate the
    -- the tax regime for a future year, based on estimated inflation.
    perPersonExemption :: Money,

    unadjustedStandardDeduction :: Integer,
    --adjustmentWhenOver65 :: Integer,
    --adjustmentWhenOldAndSingle :: Integer,

    ordinaryIncomeBrackets :: FO.OrdinaryIncomeBrackets,
    qualifiedIncomeBrackets :: FQ.QualifiedIncomeBrackets
  }
  deriving (Show)

standardDeduction :: BoundRegime -> StandardDeduction
standardDeduction br =
  StandardDeduction $ unadjustedStandardDeduction br + ageAdjustment (year br) (birthDate br)

personalExemptionDeduction :: BoundRegime -> Money
personalExemptionDeduction br =
  perPersonExemption br * fromIntegral (personalExemptions br)

netDeduction :: BoundRegime -> ItemizedDeductions -> Money
netDeduction br itemized =
  let StandardDeduction stdDed = standardDeduction br
   in personalExemptionDeduction br + max itemized (fromIntegral stdDed)

-- Note: did't seem to get adjusted for inflation.
perPersonExemptionFor :: Regime -> Year -> Money
perPersonExemptionFor NonTrump _ = 4050
perPersonExemptionFor Trump _ = 0

nonAgeAdjustedStdDeduction :: Regime -> Year -> FilingStatus -> Integer
nonAgeAdjustedStdDeduction NonTrump 2017 Single = 6350
nonAgeAdjustedStdDeduction NonTrump 2017 HeadOfHousehold = 9350
nonAgeAdjustedStdDeduction Trump 2021 Single = 12550
nonAgeAdjustedStdDeduction Trump 2021 HeadOfHousehold = 18800
nonAgeAdjustedStdDeduction r y fs = error $ printf "Unsupported combination %s, %d, %s " (show r) y (show fs)

ageAtYearEnd :: Year -> BirthDate -> Integer
ageAtYearEnd y bd =
  let (birthYear, _, _) = toGregorian bd
   in y - birthYear

ageAdjustment :: Year -> BirthDate -> Integer
ageAdjustment y bd = if ageAtYearEnd y bd > 65 then 1350 else 0

bindRegime ::
  HasCallStack =>
  Regime ->
  Year ->
  FilingStatus ->
  BirthDate ->
  PersonalExemptions ->
  BoundRegime
bindRegime Trump 2021 Single bd pes =
  BoundRegime
    Trump
    2021
    Single
    bd
    pes
    (perPersonExemptionFor Trump 2021)
    (nonAgeAdjustedStdDeduction Trump 2021 Single)
    ( FO.fromPairs
        [ (10, 0),
          (12, 9950),
          (22, 40525),
          (24, 86375),
          (32, 164925),
          (35, 209425),
          (37, 523600)
        ]
    )
    ( FQ.fromPairs
        [ (0, 0),
          (15, 40400),
          (20, 445850)
        ]
    )

bindRegime Trump 2021 HeadOfHousehold bd pes =
  BoundRegime
    Trump
    2021
    HeadOfHousehold
    bd
    pes
    (perPersonExemptionFor Trump 2021)
    (nonAgeAdjustedStdDeduction Trump 2021 HeadOfHousehold)
    ( FO.fromPairs
        [ (10, 0),
          (12, 14200),
          (22, 54200),
          (24, 86350),
          (32, 164900),
          (35, 209400),
          (37, 523600)
        ]
    )
    ( FQ.fromPairs
        [ (0, 0),
          (15, 54100),
          (20, 473850)
        ]
    )
bindRegime NonTrump 2017 Single bd pes =
  BoundRegime
    NonTrump
    2017
    Single
    bd
    pes
    (perPersonExemptionFor NonTrump 2017)
    (nonAgeAdjustedStdDeduction NonTrump 2017 Single)
    ( FO.fromPairs
        [ (10, 0),
          (15, 9235),
          (25, 37950),
          (28, 91900),
          (33, 191650),
          (35, 416700),
          (39.6, 418400)
        ]
    )
    ( FQ.fromPairs
        [ (0, 0),
          (15, 37950),
          (20, 418400)
        ]
    )
bindRegime NonTrump 2017 HeadOfHousehold bd pes =
  BoundRegime
    NonTrump
    2017
    HeadOfHousehold
    bd
    pes
    (perPersonExemptionFor NonTrump 2017)
    (nonAgeAdjustedStdDeduction NonTrump 2017 HeadOfHousehold)
    ( FO.fromPairs
        [ (10, 0),
          (15, 13350),
          (25, 50800),
          (28, 131200),
          (33, 212500),
          (35, 416700),
          (39.6, 444550)
        ]
    )
    ( FQ.fromPairs
        [ (0, 0),
          (15, 50800),
          (20, 444550)
        ]
    )
bindRegime r y fs _ _ =
  error $ printf "Unsupported combination %s, %d, %s " (show r) y (show fs)

futureEstimated :: BoundRegime -> InflationEstimate -> BoundRegime
futureEstimated br inflationEstimate =
  let InflationEstimate futureYear _ = inflationEstimate
      _ = requireRegimeValidInYear (regime br) futureYear
      factor = inflationFactor inflationEstimate (year br)
   in BoundRegime
        (regime br)
        futureYear -- TODO: is this what we want ?
        (filingStatus br)
        (birthDate br)
        (personalExemptions br)
        (perPersonExemption br * factor)
        (round $ factor * fromIntegral (unadjustedStandardDeduction br))
        (FO.inflate (ordinaryIncomeBrackets br) factor)
        (FQ.inflate (qualifiedIncomeBrackets br) factor)

