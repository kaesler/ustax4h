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
    Money,
    Year,
    inflationFactor,
    isUnmarried,
  )
import Federal.OrdinaryIncome as FO (OrdinaryIncomeBrackets, fromPairs, inflate)
import Federal.QualifiedIncome as FQ (QualifiedIncomeBrackets, fromPairs, inflate)
import Federal.Regime (Regime (PreTrump, Trump), requireRegimeValidInYear)
import Federal.Types (ItemizedDeductions, PersonalExemptions, StandardDeduction (..))
import GHC.Stack (HasCallStack)
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
    perPersonExemption :: Money,
    unadjustedStandardDeduction :: Int,
    adjustmentWhenOver65 :: Int,
    adjustmentWhenOver65AndSingle :: Int,
    ordinaryIncomeBrackets :: FO.OrdinaryIncomeBrackets,
    qualifiedIncomeBrackets :: FQ.QualifiedIncomeBrackets
  }
  deriving (Show)

standardDeduction :: BoundRegime -> StandardDeduction
standardDeduction br =
  StandardDeduction $
    unadjustedStandardDeduction br
      + ( if Age.isAge65OrOlder (birthDate br) (year br)
            then
              adjustmentWhenOver65 br
                + ( if isUnmarried (filingStatus br)
                      then adjustmentWhenOver65AndSingle br
                      else 0
                  )
            else 0
        )

personalExemptionDeduction :: BoundRegime -> Money
personalExemptionDeduction br =
  perPersonExemption br * fromIntegral (personalExemptions br)

netDeduction :: BoundRegime -> ItemizedDeductions -> Money
netDeduction br itemized =
  let StandardDeduction stdDed = standardDeduction br
   in personalExemptionDeduction br + max itemized (fromIntegral stdDed)

-- Note: does't seem to get adjusted for inflation.
perPersonExemptionFor :: Regime -> Year -> Money
perPersonExemptionFor PreTrump _ = 4050
perPersonExemptionFor Trump _ = 0

unAdjustedStdDeductionFor :: Regime -> Year -> FilingStatus -> Int
unAdjustedStdDeductionFor Trump 2022 Single = 12950
unAdjustedStdDeductionFor Trump 2022 HeadOfHousehold = 19400
unAdjustedStdDeductionFor Trump 2021 Single = 12550
unAdjustedStdDeductionFor Trump 2021 HeadOfHousehold = 18800
unAdjustedStdDeductionFor Trump 2020 HeadOfHousehold = 18650
unAdjustedStdDeductionFor Trump 2020 Single = 12400
unAdjustedStdDeductionFor Trump 2019 HeadOfHousehold = 18350
unAdjustedStdDeductionFor Trump 2019 Single = 12200
unAdjustedStdDeductionFor Trump 2018 HeadOfHousehold = 18000
unAdjustedStdDeductionFor Trump 2018 Single = 12000
unAdjustedStdDeductionFor PreTrump 2017 Single = 6350
unAdjustedStdDeductionFor PreTrump 2017 HeadOfHousehold = 9350
unAdjustedStdDeductionFor r y _ = error $ printf "Unsupported combination %s, %d " (show r) y

ageAdjustmentFor :: HasCallStack => Regime -> Year -> Int
ageAdjustmentFor Trump 2022 = 1400
ageAdjustmentFor Trump 2021 = 1350
ageAdjustmentFor Trump 2020 = 1300
ageAdjustmentFor Trump 2019 = 1300
ageAdjustmentFor Trump 2018 = 1300
ageAdjustmentFor PreTrump 2017 = 1250
ageAdjustmentFor r y = error $ printf "Unsupported combination %s, %d" (show r) y

ageAndSingleAdjustmentFor :: HasCallStack => Regime -> Year -> Int
ageAndSingleAdjustmentFor Trump 2022 = 350
ageAndSingleAdjustmentFor Trump 2021 = 350
ageAndSingleAdjustmentFor Trump 2020 = 350
ageAndSingleAdjustmentFor Trump 2019 = 350
ageAndSingleAdjustmentFor Trump 2018 = 300
ageAndSingleAdjustmentFor PreTrump 2017 = 300
ageAndSingleAdjustmentFor r y = error $ printf "Unsupported combination %s, %d" (show r) y

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
        ( FO.fromPairs
            [ (10, 0),
              (12, 14650),
              (22, 55900),
              (24, 89050),
              (32, 170050),
              (35, 215950),
              (37, 539900)
            ]
        )
        ( FQ.fromPairs
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
        ( FO.fromPairs
            [ (10, 0),
              (12, 10275),
              (22, 41775),
              (24, 89075),
              (32, 170050),
              (35, 215950),
              (37, 539900)
            ]
        )
        ( FQ.fromPairs
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
        ( FO.fromPairs
            [ (10, 0),
              (12, 14100),
              (22, 53700),
              (24, 85500),
              (32, 163300),
              (35, 207350),
              (37, 518400)
            ]
        )
        ( FQ.fromPairs
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
        ( FO.fromPairs
            [ (10, 0),
              (12, 9875),
              (22, 40125),
              (24, 85525),
              (32, 163300),
              (35, 207350),
              (37, 518400)
            ]
        )
        ( FQ.fromPairs
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
        ( FO.fromPairs
            [ (10, 0),
              (12, 13850),
              (22, 52850),
              (24, 84200),
              (32, 160700),
              (35, 204100),
              (37, 510300)
            ]
        )
        ( FQ.fromPairs
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
        ( FO.fromPairs
            [ (10, 0),
              (12, 9700),
              (22, 39475),
              (24, 84200),
              (32, 160725),
              (35, 204100),
              (37, 510300)
            ]
        )
        ( FQ.fromPairs
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
        ( FO.fromPairs
            [ (10, 0),
              (12, 13600),
              (22, 51800),
              (24, 82500),
              (32, 157500),
              (35, 200000),
              (37, 500000)
            ]
        )
        ( FQ.fromPairs
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
        ( FO.fromPairs
            [ (10, 0),
              (12, 9525),
              (22, 38700),
              (24, 82500),
              (32, 157500),
              (35, 200000),
              (37, 500000)
            ]
        )
        ( FQ.fromPairs
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
        (birthDate br)
        (filingStatus br)
        (personalExemptions br)
        (perPersonExemption br * factor)
        (round $ factor * fromIntegral (unadjustedStandardDeduction br))
        (round $ factor * fromIntegral (adjustmentWhenOver65 br))
        (round $ factor * fromIntegral (adjustmentWhenOver65AndSingle br))
        (FO.inflate (ordinaryIncomeBrackets br) factor)
        (FQ.inflate (qualifiedIncomeBrackets br) factor)
