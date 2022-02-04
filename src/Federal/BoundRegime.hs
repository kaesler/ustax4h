module Federal.BoundRegime
  ( BoundRegime (..),
    bindRegime,
    futureEstimated,
    netDeduction,
    personalExemptionDeduction,
    standardDeduction,
  )
where

import Age (isAge65OrOlder)
import CommonTypes
  ( BirthDate,
    FilingStatus (..),
    InflationEstimate (..),
    Year,
    inflationFactor,
    isUnmarried,
  )
import Data.Function ((&))
import Federal.OrdinaryBrackets as OB
  ( OrdinaryBrackets,
    fromPairs,
    inflateThresholds,
  )
import Federal.QualifiedBrackets as QB
  ( QualifiedBrackets,
    fromPairs,
    inflateThresholds,
  )
import Federal.Regime (Regime (PreTrump, Trump), requireRegimeValidInYear)
import Federal.Types (ItemizedDeductions, PersonalExemptions, StandardDeduction)
import GHC.Stack (HasCallStack)
import Moneys (Deduction, makeFromInt, mul, noMoney, times)
import Text.Printf (printf)

data BoundRegime = BoundRegime
  { --
    -- Static field: they never change.
    regime :: Regime,
    year :: Year,
    birthDate :: BirthDate,
    filingStatus :: FilingStatus,
    personalExemptions :: Int,
    --
    -- The following are inflatable. They may get adjusted to estimate the
    -- the tax regime for a future year, based on estimated inflation.
    perPersonExemption :: Deduction,
    unadjustedStandardDeduction :: Deduction,
    adjustmentWhenOver65 :: Deduction,
    adjustmentWhenOver65AndSingle :: Deduction,
    ordinaryBrackets :: OB.OrdinaryBrackets,
    qualifiedBrackets :: QB.QualifiedBrackets
  }
  deriving (Show)

standardDeduction :: BoundRegime -> StandardDeduction
standardDeduction br =
  unadjustedStandardDeduction br
    <> ( if Age.isAge65OrOlder (birthDate br) (year br)
           then
             adjustmentWhenOver65 br
               <> ( if isUnmarried (filingStatus br)
                      then adjustmentWhenOver65AndSingle br
                      else noMoney
                  )
           else noMoney
       )

personalExemptionDeduction :: BoundRegime -> Deduction
personalExemptionDeduction br = personalExemptions br `times` perPersonExemption br

netDeduction :: BoundRegime -> ItemizedDeductions -> Deduction
netDeduction br itemized =
  personalExemptionDeduction br <> max itemized (standardDeduction br)

-- Note: does't seem to get adjusted for inflation.
perPersonExemptionFor :: Regime -> Year -> Deduction
perPersonExemptionFor PreTrump _ = makeFromInt 4050
perPersonExemptionFor Trump _ = noMoney

unAdjustedStdDeductionFor :: Regime -> Year -> FilingStatus -> Deduction
unAdjustedStdDeductionFor regime year fs =
  ( case (regime, year, fs) of
      (Trump, 2022, Single) -> 12950
      (Trump, 2022, HeadOfHousehold) -> 19400
      (Trump, 2021, Single) -> 12550
      (Trump, 2021, HeadOfHousehold) -> 18800
      (Trump, 2020, Single) -> 12400
      (Trump, 2020, HeadOfHousehold) -> 18650
      (Trump, 2019, Single) -> 12200
      (Trump, 2019, HeadOfHousehold) -> 18350
      (Trump, 2018, Single) -> 12000
      (Trump, 2018, HeadOfHousehold) -> 18000
      (PreTrump, 2017, Single) -> 6350
      (PreTrump, 2017, HeadOfHousehold) -> 9350
      (r, y, _) -> error $ printf "Unsupported combination %s, %d " (show r) y
  )
    & makeFromInt

ageAdjustmentFor :: HasCallStack => Regime -> Year -> Deduction
ageAdjustmentFor regime year =
  ( case (regime, year) of
      (Trump, 2022) -> 1400
      (Trump, 2021) -> 1350
      (Trump, 2020) -> 1300
      (Trump, 2019) -> 1300
      (Trump, 2018) -> 1300
      (PreTrump, 2017) -> 1250
      (r, y) -> error $ printf "Unsupported combination %s, %d" (show r) y
  )
    & makeFromInt

ageAndSingleAdjustmentFor :: HasCallStack => Regime -> Year -> Deduction
ageAndSingleAdjustmentFor regime year =
  ( case (regime, year) of
      (Trump, 2022) -> 350
      (Trump, 2021) -> 350
      (Trump, 2020) -> 350
      (Trump, 2019) -> 350
      (Trump, 2018) -> 300
      (PreTrump, 2017) -> 300
      (r, y) -> error $ printf "Unsupported combination %s, %d" (show r) y
  )
    & makeFromInt

bindRegime ::
  HasCallStack =>
  Regime ->
  Year ->
  BirthDate ->
  FilingStatus ->
  PersonalExemptions ->
  BoundRegime
bindRegime Trump 2022 bd HeadOfHousehold pes =
  let regime = Trump
      year = 2022
      fs = HeadOfHousehold
   in BoundRegime
        regime
        year
        bd
        fs
        pes
        (perPersonExemptionFor regime year)
        (unAdjustedStdDeductionFor regime year fs)
        (ageAdjustmentFor regime year)
        (ageAndSingleAdjustmentFor regime year)
        ( OB.fromPairs
            [ (10, 0),
              (12, 14650),
              (22, 55900),
              (24, 89050),
              (32, 170050),
              (35, 215950),
              (37, 539900)
            ]
        )
        ( QB.fromPairs
            [ (0, 0),
              (15, 55800),
              (20, 488500)
            ]
        )
bindRegime Trump 2022 bd Single pes =
  let regime = Trump
      year = 2022
      fs = Single
   in BoundRegime
        regime
        year
        bd
        fs
        pes
        (perPersonExemptionFor regime year)
        (unAdjustedStdDeductionFor regime year fs)
        (ageAdjustmentFor regime year)
        (ageAndSingleAdjustmentFor regime year)
        ( OB.fromPairs
            [ (10, 0),
              (12, 10275),
              (22, 41775),
              (24, 89075),
              (32, 170050),
              (35, 215950),
              (37, 539900)
            ]
        )
        ( QB.fromPairs
            [ (0, 0),
              (15, 41675),
              (20, 459750)
            ]
        )
bindRegime Trump 2021 bd HeadOfHousehold pes =
  let regime = Trump
      year = 2021
      fs = HeadOfHousehold
   in BoundRegime
        regime
        year
        bd
        fs
        pes
        (perPersonExemptionFor regime year)
        (unAdjustedStdDeductionFor regime year fs)
        (ageAdjustmentFor regime year)
        (ageAndSingleAdjustmentFor regime year)
        ( OB.fromPairs
            [ (10, 0),
              (12, 14200),
              (22, 54200),
              (24, 86350),
              (32, 164900),
              (35, 209400),
              (37, 523600)
            ]
        )
        ( QB.fromPairs
            [ (0, 0),
              (15, 54100),
              (20, 473850)
            ]
        )
bindRegime Trump 2021 bd Single pes =
  let regime = Trump
      year = 2021
      fs = Single
   in BoundRegime
        regime
        year
        bd
        fs
        pes
        (perPersonExemptionFor regime year)
        (unAdjustedStdDeductionFor regime year fs)
        (ageAdjustmentFor regime year)
        (ageAndSingleAdjustmentFor regime year)
        ( OB.fromPairs
            [ (10, 0),
              (12, 9950),
              (22, 40525),
              (24, 86375),
              (32, 164925),
              (35, 209425),
              (37, 523600)
            ]
        )
        ( QB.fromPairs
            [ (0, 0),
              (15, 40400),
              (20, 445850)
            ]
        )
bindRegime Trump 2020 bd HeadOfHousehold pes =
  let regime = Trump
      year = 2020
      fs = HeadOfHousehold
   in BoundRegime
        regime
        year
        bd
        fs
        pes
        (perPersonExemptionFor regime year)
        (unAdjustedStdDeductionFor regime year fs)
        (ageAdjustmentFor regime year)
        (ageAndSingleAdjustmentFor regime year)
        ( OB.fromPairs
            [ (10, 0),
              (12, 14100),
              (22, 53700),
              (24, 85500),
              (32, 163300),
              (35, 207350),
              (37, 518400)
            ]
        )
        ( QB.fromPairs
            [ (0, 0),
              (15, 53600),
              (20, 469050)
            ]
        )
bindRegime Trump 2020 bd Single pes =
  let regime = Trump
      year = 2020
      fs = Single
   in BoundRegime
        regime
        year
        bd
        fs
        pes
        (perPersonExemptionFor regime year)
        (unAdjustedStdDeductionFor regime year fs)
        (ageAdjustmentFor regime year)
        (ageAndSingleAdjustmentFor regime year)
        ( OB.fromPairs
            [ (10, 0),
              (12, 9875),
              (22, 40125),
              (24, 85525),
              (32, 163300),
              (35, 207350),
              (37, 518400)
            ]
        )
        ( QB.fromPairs
            [ (0, 0),
              (15, 40000),
              (20, 442450)
            ]
        )
bindRegime Trump 2019 bd HeadOfHousehold pes =
  let regime = Trump
      year = 2019
      fs = HeadOfHousehold
   in BoundRegime
        regime
        year
        bd
        fs
        pes
        (perPersonExemptionFor regime year)
        (unAdjustedStdDeductionFor regime year fs)
        (ageAdjustmentFor regime year)
        (ageAndSingleAdjustmentFor regime year)
        ( OB.fromPairs
            [ (10, 0),
              (12, 13850),
              (22, 52850),
              (24, 84200),
              (32, 160700),
              (35, 204100),
              (37, 510300)
            ]
        )
        ( QB.fromPairs
            [ (0, 0),
              (15, 52750),
              (20, 461700)
            ]
        )
bindRegime Trump 2019 bd Single pes =
  let regime = Trump
      year = 2019
      fs = Single
   in BoundRegime
        regime
        year
        bd
        fs
        pes
        (perPersonExemptionFor regime year)
        (unAdjustedStdDeductionFor regime year fs)
        (ageAdjustmentFor regime year)
        (ageAndSingleAdjustmentFor regime year)
        ( OB.fromPairs
            [ (10, 0),
              (12, 9700),
              (22, 39475),
              (24, 84200),
              (32, 160725),
              (35, 204100),
              (37, 510300)
            ]
        )
        ( QB.fromPairs
            [ (0, 0),
              (15, 39375),
              (20, 434550)
            ]
        )
bindRegime Trump 2018 bd HeadOfHousehold pes =
  let regime = Trump
      year = 2018
      fs = HeadOfHousehold
   in BoundRegime
        regime
        year
        bd
        fs
        pes
        (perPersonExemptionFor regime year)
        (unAdjustedStdDeductionFor regime year fs)
        (ageAdjustmentFor regime year)
        (ageAndSingleAdjustmentFor regime year)
        ( OB.fromPairs
            [ (10, 0),
              (12, 13600),
              (22, 51800),
              (24, 82500),
              (32, 157500),
              (35, 200000),
              (37, 500000)
            ]
        )
        ( QB.fromPairs
            [ (0, 0),
              (15, 51700),
              (20, 452400)
            ]
        )
bindRegime Trump 2018 bd Single pes =
  let regime = Trump
      year = 2018
      fs = Single
   in BoundRegime
        regime
        year
        bd
        fs
        pes
        (perPersonExemptionFor regime year)
        (unAdjustedStdDeductionFor regime year fs)
        (ageAdjustmentFor regime year)
        (ageAndSingleAdjustmentFor regime year)
        ( OB.fromPairs
            [ (10, 0),
              (12, 9525),
              (22, 38700),
              (24, 82500),
              (32, 157500),
              (35, 200000),
              (37, 500000)
            ]
        )
        ( QB.fromPairs
            [ (0, 0),
              (15, 38600),
              (20, 425800)
            ]
        )
bindRegime PreTrump 2017 bd HeadOfHousehold pes =
  let regime = PreTrump
      year = 2017
      fs = HeadOfHousehold
   in BoundRegime
        regime
        year
        bd
        fs
        pes
        (perPersonExemptionFor regime year)
        (unAdjustedStdDeductionFor regime year fs)
        (ageAdjustmentFor regime year)
        (ageAndSingleAdjustmentFor regime year)
        ( OB.fromPairs
            [ (10, 0),
              (15, 13350),
              (25, 50800),
              (28, 131200),
              (33, 212500),
              (35, 416700),
              (39.6, 444550)
            ]
        )
        ( QB.fromPairs
            [ (0, 0),
              (15, 50800),
              (20, 444550)
            ]
        )
bindRegime PreTrump 2017 bd Single pes =
  let regime = PreTrump
      year = 2017
      fs = Single
   in BoundRegime
        regime
        year
        bd
        fs
        pes
        (perPersonExemptionFor regime year)
        (unAdjustedStdDeductionFor regime year fs)
        (ageAdjustmentFor regime year)
        (ageAndSingleAdjustmentFor regime year)
        ( OB.fromPairs
            [ (10, 0),
              (15, 9325),
              (25, 37950),
              (28, 91900),
              (33, 191650),
              (35, 416700),
              (39.6, 418400)
            ]
        )
        ( QB.fromPairs
            [ (0, 0),
              (15, 37950),
              (20, 418400)
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
        futureYear
        (birthDate br)
        (filingStatus br)
        (personalExemptions br)
        (perPersonExemption br `mul` factor)
        (unadjustedStandardDeduction br `mul` factor)
        (adjustmentWhenOver65 br `mul` factor)
        (adjustmentWhenOver65AndSingle br `mul` factor)
        (OB.inflateThresholds factor (ordinaryBrackets br))
        (QB.inflateThresholds factor (qualifiedBrackets br))
