module Federal.BoundRegime
  ( BoundRegime (..),
    boundRegimeForKnownYear,
    boundRegimeForFutureYear,
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
import Federal.OrdinaryBrackets as OB
  ( OrdinaryBrackets,
    inflateThresholds,
  )
import Federal.QualifiedBrackets as QB
  ( QualifiedBrackets,
    inflateThresholds,
  )
import Federal.Regime (Regime)
import Federal.Types (ItemizedDeductions, PersonalExemptions, StandardDeduction)
import qualified Federal.Yearly.YearlyValues as YV
import Moneys (Deduction, mul, noMoney, times)

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

boundRegimeForKnownYear :: Year -> BirthDate -> FilingStatus -> PersonalExemptions -> BoundRegime
boundRegimeForKnownYear y bd fs pe =
  let yvs = YV.unsafeValuesForYear y
   in BoundRegime
        (YV.regime yvs)
        y
        bd
        fs
        pe
        (YV.perPersonExemption yvs)
        (YV.unadjustedStandardDeduction yvs fs)
        (YV.adjustmentWhenOver65 yvs)
        (YV.adjustmentWhenOver65AndSingle yvs)
        (YV.ordinaryBrackets yvs fs)
        (YV.qualifiedBrackets yvs fs)

boundRegimeForFutureYear :: Regime -> InflationEstimate -> BirthDate -> FilingStatus -> PersonalExemptions -> BoundRegime
boundRegimeForFutureYear reg estimate bd fs pe =
  let baseYear = YV.mostRecentYearForRegime reg
   in futureEstimated (boundRegimeForKnownYear baseYear bd fs pe) estimate

futureEstimated :: BoundRegime -> InflationEstimate -> BoundRegime
futureEstimated br inflationEstimate =
  let InflationEstimate futureYear _ = inflationEstimate
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
