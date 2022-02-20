module Federal.Yearly.YearlyValues
  ( YearlyValues (..),
    unsafeValuesForYear,
    valuesForYear,
    mostRecent,
    mostRecentForRegime,
    mostRecentYearForRegime,
  )
where

import CommonTypes (Year)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NEList
import Data.Map.NonEmpty (NEMap, toAscList)
import qualified Data.Map.NonEmpty as NEMap
import Data.Maybe (fromJust)
import Federal.Regime (Regime)
import Federal.Yearly.Type (YearlyValues (..))
import qualified Federal.Yearly.Year2016 as Year2016
import qualified Federal.Yearly.Year2017 as Year2017
import qualified Federal.Yearly.Year2018 as Year2018
import qualified Federal.Yearly.Year2019 as Year2019
import qualified Federal.Yearly.Year2020 as Year2020
import qualified Federal.Yearly.Year2021 as Year2021
import qualified Federal.Yearly.Year2022 as Year2022

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
