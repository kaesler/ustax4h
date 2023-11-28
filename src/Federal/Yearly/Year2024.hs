{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Federal.Yearly.Year2024
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
      year = 2024,
      perPersonExemption = makeFromInt 0,
      unadjustedStandardDeduction =
        \case
          Married -> makeFromInt 29200
          HeadOfHousehold -> makeFromInt 21900
          Single -> makeFromInt 14600,
      adjustmentWhenOver65 = makeFromInt 1550,
      adjustmentWhenOver65AndSingle = makeFromInt 400,
      ordinaryBrackets =
        \case
          Married ->
            OB.fromPairs
              [ (0, 10),
                (23200, 12),
                (94300, 22),
                (201050, 24),
                (383900, 32),
                (487450, 35),
                (731200, 37)
              ]
          HeadOfHousehold ->
            OB.fromPairs
              [ (0, 10),
                (16550, 12),
                (63100, 22),
                (100500, 24),
                (191950, 32),
                (243700, 35),
                (609350, 37)
              ]
          Single ->
            OB.fromPairs
              [ (0, 10),
                (11600, 12),
                (47150, 22),
                (100525, 24),
                (191950, 32),
                (243725, 35),
                (609350, 37)
              ],
      qualifiedBrackets =
        \case
          Married ->
            QB.fromPairs
              [ (0, 0),
                (94050, 15),
                (583750, 20)
              ]
          HeadOfHousehold ->
            QB.fromPairs
              [ (0, 0),
                (63000, 15),
                (551350, 20)
              ]
          Single ->
            QB.fromPairs
              [ (0, 0),
                (47025, 15),
                (518900, 20)
              ]
    }
