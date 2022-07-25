module Federal.BoundRegime
  ( BoundRegime (..),
    boundRegimeForKnownYear,
    boundRegimeForFutureYear,
    netDeduction,
    personalExemptionDeduction,
    standardDeduction,
    withEstimatedNetInflationFactor,
  )
where

import Age (isAge65OrOlder)
import CommonTypes
  ( BirthDate,
    FilingStatus (..),
    Year,
    isUnmarried,
  )
import Data.Maybe (fromMaybe)
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

boundRegimeForFutureYear :: Regime -> Year -> Double -> BirthDate -> FilingStatus -> PersonalExemptions -> BoundRegime
boundRegimeForFutureYear r y annualInflationFactor bd fs pe =
  let baseValues = YV.mostRecentForRegime r
      baseYear = YV.year baseValues
      baseRegime = boundRegimeForKnownYear y bd fs pe
      yearsWithInflation = [(baseYear + 1) .. y]
      inflationFactors =
        do
          ywi <- yearsWithInflation
          let factor = fromMaybe annualInflationFactor $ YV.averageThresholdChangeOverPrevious ywi
          return factor
      netInflationFactor = product inflationFactors
   in withEstimatedNetInflationFactor y netInflationFactor baseRegime

withEstimatedNetInflationFactor :: Year -> Double -> BoundRegime -> BoundRegime
withEstimatedNetInflationFactor futureYear netInflationFactor br =
  BoundRegime
    (regime br)
    futureYear
    (birthDate br)
    (filingStatus br)
    (personalExemptions br)
    (perPersonExemption br `mul` netInflationFactor)
    (unadjustedStandardDeduction br `mul` netInflationFactor)
    (adjustmentWhenOver65 br `mul` netInflationFactor)
    (adjustmentWhenOver65AndSingle br `mul` netInflationFactor)
    (OB.inflateThresholds netInflationFactor (ordinaryBrackets br))
    (QB.inflateThresholds netInflationFactor (qualifiedBrackets br))
