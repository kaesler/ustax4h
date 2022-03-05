{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Federal.Yearly.Year2019
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
    { regime = Trump,
      year = 2019,
      perPersonExemption = makeFromInt 0,
      unadjustedStandardDeduction =
        \case
          Married -> makeFromInt 24400
          HeadOfHousehold -> makeFromInt 18350
          Single -> makeFromInt 12200,
      adjustmentWhenOver65 = makeFromInt 1300,
      adjustmentWhenOver65AndSingle = makeFromInt 350,
      ordinaryBrackets =
        \case
          Married ->
            OB.fromPairs
              [ (0, 10),
                (19400, 12),
                (78950, 22),
                (168400, 24),
                (321450, 32),
                (408200, 35),
                (612350, 37)
              ]
          HeadOfHousehold ->
            OB.fromPairs
              [ (0, 10),
                (13850, 12),
                (52850, 22),
                (84200, 24),
                (160700, 32),
                (204100, 35),
                (510300, 37)
              ]
          Single ->
            OB.fromPairs
              [ (0, 10),
                (9700, 12),
                (39475, 22),
                (84200, 24),
                (160725, 32),
                (204100, 35),
                (510300, 37)
              ],
      qualifiedBrackets =
        \case
          Married ->
            QB.fromPairs
              [ (0, 0),
                (83350, 15),
                (517200, 20)
              ]
          HeadOfHousehold ->
            QB.fromPairs
              [ (0, 0),
                (52750, 15),
                (461700, 20)
              ]
          Single ->
            QB.fromPairs
              [ (0, 0),
                (39375, 15),
                (434550, 20)
              ]
    }
