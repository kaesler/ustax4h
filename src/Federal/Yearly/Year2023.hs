{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Federal.Yearly.Year2023
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
      year = 2023,
      perPersonExemption = makeFromInt 0,
      unadjustedStandardDeduction =
        \case
          MarriedJoint -> makeFromInt 27700
          HeadOfHousehold -> makeFromInt 20800
          Single -> makeFromInt 13850,
      adjustmentWhenOver65 = makeFromInt 1500,
      adjustmentWhenOver65AndSingle = makeFromInt 350,
      ordinaryBrackets =
        \case
          MarriedJoint ->
            OB.fromPairs
              [ (0, 10),
                (22000, 12),
                (89450, 22),
                (190750, 24),
                (364200, 32),
                (462500, 35),
                (693750, 37)
              ]
          HeadOfHousehold ->
            OB.fromPairs
              [ (0, 10),
                (15700, 12),
                (59850, 22),
                (95350, 24),
                (182100, 32),
                (231250, 35),
                (578100, 37)
              ]
          Single ->
            OB.fromPairs
              [ (0, 10),
                (11000, 12),
                (44725, 22),
                (95375, 24),
                (182100, 32),
                (231250, 35),
                (578125, 37)
              ],
      qualifiedBrackets =
        \case
          MarriedJoint ->
            QB.fromPairs
              [ (0, 0),
                (89250, 15),
                (553850, 20)
              ]
          HeadOfHousehold ->
            QB.fromPairs
              [ (0, 0),
                (59750, 15),
                (523050, 20)
              ]
          Single ->
            QB.fromPairs
              [ (0, 0),
                (44625, 15),
                (492300, 20)
              ]
    }
