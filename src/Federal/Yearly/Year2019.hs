{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Federal.Yearly.Year2019
  ( values,
  )
where

import CommonTypes (FilingStatus (HeadOfHousehold, Single))
import Federal.OrdinaryBrackets as OB
  ( OrdinaryBrackets,
    fromPairs,
    fromRPairs,
  )
import Federal.QualifiedBrackets as QB
  ( QualifiedBrackets,
    fromPairs,
    fromRPairs,
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
          HeadOfHousehold -> makeFromInt 18350
          Single -> makeFromInt 12200,
      adjustmentWhenOver65 = makeFromInt 1300,
      adjustmentWhenOver65AndSingle = makeFromInt 350,
      ordinaryBrackets =
        \case
          HeadOfHousehold ->
            OB.fromRPairs
              [ (0, 10),
                (13850, 12),
                (52850, 22),
                (84200, 24),
                (160700, 32),
                (204100, 35),
                (510300, 37)
              ]
          Single ->
            OB.fromRPairs
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
          HeadOfHousehold ->
            QB.fromRPairs
              [ (0, 0),
                (52750, 15),
                (461700, 20)
              ]
          Single ->
            QB.fromRPairs
              [ (0, 0),
                (39375, 15),
                (434550, 20)
              ]
    }