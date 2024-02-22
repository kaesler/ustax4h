module Federal.Yearly.YearlyValues
  ( YearlyValues (..),
    averageThresholdChange,
    averageThresholdChangeOverPrevious,
    unsafeValuesForYear,
    valuesForYear,
    mostRecent,
    mostRecentForRegime,
    mostRecentYearForRegime,
    ordinaryNonZeroThresholdsMap,
    qualifiedNonZeroThresholdsMap,
    previous,
    haveCongruentOrdinaryBrackets,
    haveCongruentQualifiedBrackets,
  )
where

import CommonTypes (FilingStatus (..), Year)
import Data.List.NonEmpty (NonEmpty, toList)
import qualified Data.List.NonEmpty as NEList
import Data.Map.NonEmpty (NEMap, toAscList)
import qualified Data.Map.NonEmpty as NEMap
import qualified Data.Map.Strict as Map
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
import qualified Federal.Yearly.Year2023 as Year2023
import qualified Federal.Yearly.Year2024 as Year2024
import Moneys (IncomeThreshold, divide, nonZero)

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
        (2022, Year2022.values),
        (2023, Year2023.values),
        (2024, Year2024.values)
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

ordinaryNonZeroThresholdsMap :: YearlyValues -> Map.Map (FilingStatus, FederalTaxRate) IncomeThreshold
ordinaryNonZeroThresholdsMap yv =
  let pairs = do
        fs <- [Single, HeadOfHousehold, MarriedJoint]
        let obs = ordinaryBrackets yv fs
        (rate, threshold) <- filter (\(_, t) -> nonZero t) $ NEList.toList $ OB.toPairs obs
        return ((fs, rate), threshold)
   in Map.fromList pairs

qualifiedNonZeroThresholdsMap :: YearlyValues -> Map.Map (FilingStatus, FederalTaxRate) IncomeThreshold
qualifiedNonZeroThresholdsMap yv =
  let pairs = do
        fs <- [Single, HeadOfHousehold, MarriedJoint]
        let qbs = qualifiedBrackets yv fs
        (rate, threshold) <- filter (\(_, t) -> nonZero t) $ NEList.toList $ QB.toPairs qbs
        return ((fs, rate), threshold)
   in Map.fromList pairs

haveCongruentOrdinaryBrackets :: YearlyValues -> YearlyValues -> Bool
haveCongruentOrdinaryBrackets left right =
  (Map.keysSet $ ordinaryNonZeroThresholdsMap left) == (Map.keysSet $ ordinaryNonZeroThresholdsMap right)

haveCongruentQualifiedBrackets :: YearlyValues -> YearlyValues -> Bool
haveCongruentQualifiedBrackets left right =
  (Map.keysSet $ qualifiedNonZeroThresholdsMap left) == (Map.keysSet $ qualifiedNonZeroThresholdsMap right)

averageThresholdChange :: YearlyValues -> YearlyValues -> Double
averageThresholdChange left right =
  let -- Note: these lists are sorted ascending by associated key order.
      ordPairs
        | (haveCongruentOrdinaryBrackets left right) =
          zip (Map.elems (ordinaryNonZeroThresholdsMap left)) (Map.elems (ordinaryNonZeroThresholdsMap right))
        | otherwise = []
      qualPairs
        | (haveCongruentQualifiedBrackets left right) =
          zip (Map.elems (qualifiedNonZeroThresholdsMap left)) (Map.elems (qualifiedNonZeroThresholdsMap right))
        | otherwise = []
      pairs = ordPairs ++ qualPairs
      changes = fmap (\(l, r) -> r `divide` l) pairs
      averageChange = (sum changes) / (fromIntegral (length changes))
   in averageChange

memoizedAverageThresholdChanges :: Map.Map Year Double
memoizedAverageThresholdChanges =
  let yvs = toList $ NEMap.elems forYear
      yvPairs = zip yvs $ tail yvs
      mapPairs = fmap (\(left, right) -> ((year right), averageThresholdChange left right)) yvPairs
   in Map.fromList mapPairs

averageThresholdChangeOverPrevious :: Year -> Maybe Double
averageThresholdChangeOverPrevious y = Map.lookup y memoizedAverageThresholdChanges
