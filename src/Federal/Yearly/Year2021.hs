{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Federal.Yearly.Year2021
  ( values,
  )
where

import CommonTypes (FilingStatus (..))
import Federal.OrdinaryBrackets as OB
  ( fromPairs,
  )
import Federal.QualifiedBrackets as QB
  ( fromPairs,
  )
import Federal.Regime (Regime (..))
import Federal.Yearly.Type (YearlyValues (..))
import Moneys (makeFromInt)

values :: YearlyValues
values =
  YearlyValues
    { regime = TCJA,
      year = 2021,
      perPersonExemption = makeFromInt 0,
      unadjustedStandardDeduction =
        \case
          MarriedJoint -> makeFromInt 25100
          HeadOfHousehold -> makeFromInt 18800
          Single -> makeFromInt 12550,
      adjustmentWhenOver65 = makeFromInt 1350,
      adjustmentWhenOver65AndSingle = makeFromInt 350,
      ordinaryBrackets =
        \case
          MarriedJoint ->
            OB.fromPairs
              [ (0, 10),
                (19900, 12),
                (81050, 22),
                (172750, 24),
                (329850, 32),
                (418850, 35),
                (628300, 37)
              ]
          HeadOfHousehold ->
            OB.fromPairs
              [ (0, 10),
                (14200, 12),
                (54200, 22),
                (86350, 24),
                (164900, 32),
                (209400, 35),
                (523600, 37)
              ]
          Single ->
            OB.fromPairs
              [ (0, 10),
                (9950, 12),
                (40525, 22),
                (86375, 24),
                (164925, 32),
                (209425, 35),
                (523600, 37)
              ],
      qualifiedBrackets =
        \case
          MarriedJoint ->
            QB.fromPairs
              [ (0, 0),
                (80800, 15),
                (501600, 20)
              ]
          HeadOfHousehold ->
            QB.fromPairs
              [ (0, 0),
                (54100, 15),
                (473750, 20)
              ]
          Single ->
            QB.fromPairs
              [ (0, 0),
                (40400, 15),
                (445850, 20)
              ]
    }
