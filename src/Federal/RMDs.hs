module Federal.RMDs
  ( rmdFractionForAge,
  )
where

import CommonTypes
import Data.List.NonEmpty as NonEmpty (fromList)
import Data.Map.NonEmpty as NEMap (NEMap, fromList, lookup)
import Data.Maybe (fromJust)

rmdFractionForAge :: Age -> Double
rmdFractionForAge age = 1.0 / fromJust (NEMap.lookup age distributionPeriods)

distributionPeriods :: NEMap Age DistributionPeriod
distributionPeriods =
  NEMap.fromList $
    NonEmpty.fromList
      [ (Age 70, 27.4),
        (Age 71, 26.5),
        (Age 72, 25.6),
        (Age 73, 24.7),
        (Age 74, 23.8),
        (Age 75, 22.9),
        (Age 76, 22.0),
        (Age 77, 21.2),
        (Age 78, 20.3),
        (Age 79, 19.5),
        (Age 80, 18.7),
        (Age 81, 17.9),
        (Age 82, 17.1),
        (Age 83, 16.3),
        (Age 84, 15.5),
        (Age 85, 14.8),
        (Age 86, 14.1),
        (Age 87, 13.4),
        (Age 88, 12.7),
        (Age 89, 12.0),
        (Age 90, 11.4),
        (Age 91, 10.8),
        (Age 92, 10.2),
        (Age 93, 9.6),
        (Age 94, 9.1),
        (Age 95, 8.6),
        (Age 96, 8.1),
        (Age 97, 7.6),
        (Age 98, 7.1),
        (Age 99, 6.7),
        (Age 100, 6.3),
        (Age 101, 5.9),
        (Age 102, 5.5),
        (Age 103, 5.2),
        (Age 104, 4.9),
        (Age 105, 4.5),
        (Age 106, 4.2),
        (Age 107, 3.9),
        (Age 108, 3.7),
        (Age 109, 3.4),
        (Age 110, 3.1),
        (Age 111, 2.9),
        (Age 112, 2.6),
        (Age 113, 2.4),
        (Age 114, 2.1)
      ]
