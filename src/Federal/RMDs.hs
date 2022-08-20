module Federal.RMDs
  ( fractionForAge,
  )
where

import CommonTypes (Age (..))
import Data.List.NonEmpty as NonEmpty (fromList)
import Data.Map.NonEmpty as NEMap (NEMap, fromList, lookup)
import Data.Maybe (fromJust)
import Federal.Types (DistributionPeriod)

fractionForAge :: Age -> Double
fractionForAge age
  | age < Age 72 = 0.0
  | age > Age 120 = 0.5
  | otherwise = 1.0 / fromJust (NEMap.lookup age distributionPeriods)

distributionPeriods :: NEMap Age DistributionPeriod
distributionPeriods =
  NEMap.fromList $
    NonEmpty.fromList
      [ (Age 72, 27.4),
        (Age 73, 26.5),
        (Age 74, 25.5),
        (Age 75, 24.6),
        (Age 76, 23.7),
        (Age 77, 22.9),
        (Age 78, 22.0),
        (Age 79, 21.1),
        (Age 80, 20.2),
        (Age 81, 19.4),
        (Age 82, 18.5),
        (Age 83, 17.7),
        (Age 84, 16.8),
        (Age 85, 16.0),
        (Age 86, 15.2),
        (Age 87, 14.4),
        (Age 88, 13.7),
        (Age 89, 12.9),
        (Age 90, 12.2),
        (Age 91, 11.5),
        (Age 92, 10.8),
        (Age 93, 10.1),
        (Age 94, 9.5),
        (Age 95, 8.9),
        (Age 96, 8.4),
        (Age 97, 7.8),
        (Age 98, 7.3),
        (Age 99, 6.8),
        (Age 100, 6.4),
        (Age 101, 6.0),
        (Age 102, 5.6),
        (Age 103, 5.2),
        (Age 104, 4.9),
        (Age 105, 4.6),
        (Age 106, 4.3),
        (Age 107, 4.1),
        (Age 108, 3.9),
        (Age 109, 3.7),
        (Age 110, 3.5),
        (Age 111, 3.4),
        (Age 112, 3.3),
        (Age 113, 3.1),
        (Age 114, 3.0),
        (Age 115, 2.9),
        (Age 116, 2.8),
        (Age 117, 2.7),
        (Age 118, 2.5),
        (Age 119, 2.3),
        (Age 120, 2.0)
      ]
