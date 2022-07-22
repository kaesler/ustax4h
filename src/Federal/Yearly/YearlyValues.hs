module Federal.Yearly.YearlyValues
  ( YearlyValues (..),
    unsafeValuesForYear,
    valuesForYear,
    mostRecent,
    mostRecentForRegime,
    mostRecentYearForRegime,
    ordinaryNonZeroThresholdsMap,
    qualifiedNonZeroThresholdsMap,
    previous
  )
where

import CommonTypes (FilingStatus(..), Year)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NEList
import Data.Map (Map, fromList)
import Data.Map.NonEmpty (NEMap, toAscList)
import qualified Data.Map.NonEmpty as NEMap
import Data.Maybe (fromJust)
import Federal.FederalTaxRate (FederalTaxRate)
import qualified Federal.OrdinaryBrackets as OB
import qualified Federal.QualifiedBrackets as QB
import Federal.Regime (Regime)
import Federal.Yearly.Type (YearlyValues (..))
import qualified Federal.Yearly.Year2016 as Year2016
import qualified Federal.Yearly.Year2017 as Year2017
import qualified Federal.Yearly.Year2018 as Year2018
import qualified Federal.Yearly.Year2019 as Year2019
import qualified Federal.Yearly.Year2020 as Year2020
import qualified Federal.Yearly.Year2021 as Year2021
import qualified Federal.Yearly.Year2022 as Year2022
import Moneys (IncomeThreshold, nonZero)

forYear :: NEMap Year YearlyValues
forYear =
  NEMap.fromList $
    NEList.fromList
      [ (2016, Year2016.values),
        (2017, Year2017.values),
        (2018, Year2018.values),
        (2019, Year2019.values),
        (2020, Year2020.values),
        (2021, Year2021.values),
        (2022, Year2022.values)
      ]

unsafeValuesForYear :: Year -> YearlyValues
unsafeValuesForYear y = fromJust $ NEMap.lookup y forYear

valuesForYear :: Year -> Maybe YearlyValues
valuesForYear y = NEMap.lookup y forYear

mostRecent :: YearlyValues
mostRecent = NEList.last valuesAscendingByYear

mostRecentForRegime :: Regime -> YearlyValues
mostRecentForRegime reg = last $ valuesAscendingByYearForRegime reg

mostRecentYearForRegime :: Regime -> Year
mostRecentYearForRegime reg = year $ mostRecentForRegime reg

valuesAscendingByYear :: NonEmpty YearlyValues
valuesAscendingByYear = snd <$> toAscList forYear

valuesAscendingByYearForRegime :: Regime -> [YearlyValues]
valuesAscendingByYearForRegime reg =
  NEList.filter (\yv -> regime yv == reg) valuesAscendingByYear

previous :: YearlyValues -> Maybe YearlyValues
previous yv = valuesForYear $ (year yv) - 1

ordinaryNonZeroThresholdsMap :: YearlyValues -> Map (FilingStatus, FederalTaxRate) IncomeThreshold
ordinaryNonZeroThresholdsMap yv = 
  let pairs = do fs <- [Single, HeadOfHousehold, Married]
                 let obs = ordinaryBrackets yv fs
                 (rate, threshold) <- filter (\(_, t) -> nonZero t) $ NEList.toList $ OB.toPairs obs
                 return ((fs, rate), threshold)
  in 
    fromList pairs
  
qualifiedNonZeroThresholdsMap :: YearlyValues -> Map (FilingStatus, FederalTaxRate) IncomeThreshold
qualifiedNonZeroThresholdsMap yv = 
  let pairs = do fs <- [Single, HeadOfHousehold, Married]
                 let qbs = qualifiedBrackets yv fs
                 (rate, threshold) <- filter (\(_, t) -> nonZero t) $ NEList.toList $ QB.toPairs qbs
                 return ((fs, rate), threshold)
  in 
    fromList pairs
  
