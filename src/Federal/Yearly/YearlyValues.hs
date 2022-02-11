module Federal.Yearly.YearlyValues
  (
  )
where

import CommonTypes (FilingStatus, Year)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.NonEmpty (NEMap)
import qualified Data.Map.NonEmpty as NEMap
import Federal.OrdinaryBrackets (OrdinaryBrackets)
import Federal.QualifiedBrackets (QualifiedBrackets)
import Federal.Regime (Regime)
import Federal.Yearly.Type (YearlyValues)
import qualified Federal.Yearly.Year2016 as Year2016
import qualified Federal.Yearly.Year2017 as Year2017
import qualified Federal.Yearly.Year2018 as Year2018
import qualified Federal.Yearly.Year2019 as Year2019
import qualified Federal.Yearly.Year2020 as Year2020
import qualified Federal.Yearly.Year2021 as Year2021
import qualified Federal.Yearly.Year2022 as Year2022
import Moneys (Deduction)

valuesForYear :: Year -> Maybe YearlyValues
valuesForYear = undefined

last :: YearlyValues
last = undefined

lastForRegime :: Regime -> YearlyValues
lastForRegime = undefined

all :: NEMap Year YearlyValues
all =
  NEMap.fromList $
    NonEmpty.fromList
      [ (2016, Year2016.values),
        (2017, Year2017.values),
        (2018, Year2018.values),
        (2019, Year2019.values),
        (2020, Year2020.values),
        (2021, Year2021.values),
        (2022, Year2022.values)
      ]
