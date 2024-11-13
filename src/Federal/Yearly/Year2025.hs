{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Federal.Yearly.Year2025
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
      year = 2025,
      perPersonExemption = makeFromInt 0,
      unadjustedStandardDeduction =
        \case
          MarriedJoint -> makeFromInt 30000
          HeadOfHousehold -> makeFromInt 22500
          Single -> makeFromInt 15000,
      adjustmentWhenOver65 = makeFromInt 1600,
      adjustmentWhenOver65AndSingle = makeFromInt 400,
      ordinaryBrackets =
        \case
          MarriedJoint ->
            OB.fromPairs
              [ (0, 10),
                (23850, 12),
                (96950, 22),
                (206700, 24),
                (394600, 32),
                (501050, 35),
                (751600, 37)
              ]
          HeadOfHousehold ->
            OB.fromPairs
              [ (0, 10),
                (17000, 12),
                (64850, 22),
                (103350, 24),
                (197300, 32),
                (250500, 35),
                (626350, 37)
              ]
          Single ->
            OB.fromPairs
              [ (0, 10),
                (11925, 12),
                (48475, 22),
                (103350, 24),
                (197300, 32),
                (250525, 35),
                (626350, 37)
              ],
      qualifiedBrackets =
        \case
          MarriedJoint ->
            QB.fromPairs
              [ (0, 0),
                (96700, 15),
                (600050, 20)
              ]
          HeadOfHousehold ->
            QB.fromPairs
              [ (0, 0),
                (64750, 15),
                (566700, 20)
              ]
          Single ->
            QB.fromPairs
              [ (0, 0),
                (48350, 15),
                (533400, 20)
              ]
    }
