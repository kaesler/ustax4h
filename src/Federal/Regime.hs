module Federal.Regime
  ( Regime (..),
    BoundRegime (..),
    bindRegime,
    futureEstimated,
    netDeduction,
    personalExemptionDeduction,
    standardDeduction,
  )
where

import CommonTypes
  ( BirthDate,
    FilingStatus (..),
    InflationEstimate (..),
    ItemizedDeductions,
    Money,
    PersonalExemptions,
    StandardDeduction (..),
    Year,
    inflationFactor,
  )
import Data.Time (toGregorian)
import Federal.BracketTypes ()
import qualified Federal.OrdinaryIncome as FO
  ( OrdinaryIncomeBrackets,
    fromPairs,
    inflate,
  )
import qualified Federal.QualifiedIncome as FQ
  ( QualifiedIncomeBrackets,
    fromPairs,
    inflate,
  )
import GHC.Stack (HasCallStack)
import Text.Printf

data Regime = Trump | NonTrump
  deriving (Eq, Ord, Show, Enum)

requireRegimeValidInYear :: Regime -> Year -> ()
requireRegimeValidInYear r y =
  if regimeValidInYear r y
    then ()
    else error $ printf "Regime %s not valid in year %d" (show r) y

regimeValidInYear :: Regime -> Year -> Bool
regimeValidInYear Trump y = y >= 2018
regimeValidInYear NonTrump y = y < 2018 || y > 2025

-- TODO: decide what value fields can/should be inflated. E.g. do we need
-- unadjustedStdDeduction there? perPersonExemption ?
-- self-ref function fields should not be inflated.
-- Should we include target year too so age adjustment can change with future projection?
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

mkBoundRegime ::
  Regime ->
  Year ->
  FilingStatus ->
  BirthDate ->
  PersonalExemptions ->
  Money ->
  Integer ->
  FO.OrdinaryIncomeBrackets ->
  FQ.QualifiedIncomeBrackets ->
  BoundRegime
mkBoundRegime r y fs bd pe ppe uasd ob qb =
  -- Note: Self-reference to allow some fields to depend on others.
  let self =
        BoundRegime
          { regime = r,
            year = y,
            filingStatus = fs,
            birthDate = bd,
            personalExemptions = pe,
            perPersonExemption = ppe,
            unadjustedStandardDeduction = uasd,
            ordinaryIncomeBrackets = ob,
            qualifiedIncomeBrackets = qb
          }
   in self

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
  mkBoundRegime
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
  mkBoundRegime
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
  mkBoundRegime
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
  mkBoundRegime
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
   in mkBoundRegime
        (regime br)
        futureYear -- TODO: is this what we want ?
        (filingStatus br)
        (birthDate br)
        (personalExemptions br)
        (perPersonExemption br * factor)
        (round $ factor * fromIntegral (unadjustedStandardDeduction br))
        (FO.inflate (ordinaryIncomeBrackets br) factor)
        (FQ.inflate (qualifiedIncomeBrackets br) factor)
