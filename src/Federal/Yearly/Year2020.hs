{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Federal.Yearly.Year2020
  ( values,
  )
where

import CommonTypes (FilingStatus (HeadOfHousehold, Single))
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
      year = 2020,
      perPersonExemption = makeFromInt 0,
      unadjustedStandardDeduction =
        \case
          HeadOfHousehold -> makeFromInt 18650
          Single -> makeFromInt 12400,
      adjustmentWhenOver65 = makeFromInt 1300,
      adjustmentWhenOver65AndSingle = makeFromInt 350,
      ordinaryBrackets =
        \case
          HeadOfHousehold ->
            OB.fromPairs
              [ (0, 10),
                (14100, 12),
                (53700, 22),
                (85500, 24),
                (163300, 32),
                (207350, 35),
                (518400, 37)
              ]
          Single ->
            OB.fromPairs
              [ (0, 10),
                (9875, 12),
                (40125, 22),
                (85525, 24),
                (163300, 32),
                (207350, 35),
                (518400, 37)
              ],
      qualifiedBrackets =
        \case
          HeadOfHousehold ->
            QB.fromPairs
              [ (0, 0),
                (53600, 15),
                (469050, 20)
              ]
          Single ->
            QB.fromPairs
              [ (0, 0),
                (40000, 15),
                (442450, 20)
              ]
    }
