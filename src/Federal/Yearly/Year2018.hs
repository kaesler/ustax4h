{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Federal.Yearly.Year2018
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
      year = 2018,
      perPersonExemption = makeFromInt 0,
      unadjustedStandardDeduction =
        \case
          Married -> makeFromInt 24000
          HeadOfHousehold -> makeFromInt 18000
          Single -> makeFromInt 12000,
      adjustmentWhenOver65 = makeFromInt 1300,
      adjustmentWhenOver65AndSingle = makeFromInt 300,
      ordinaryBrackets =
        \case
          Married ->
            OB.fromPairs
              [ (0, 10),
                (19050, 12),
                (77400, 22),
                (165000, 24),
                (315000, 32),
                (400000, 35),
                (600000, 37)
              ]
          HeadOfHousehold ->
            OB.fromPairs
              [ (0, 10),
                (13600, 12),
                (51800, 22),
                (82500, 24),
                (157500, 32),
                (200000, 35),
                (500000, 37)
              ]
          Single ->
            OB.fromPairs
              [ (0, 10),
                (9525, 12),
                (38700, 22),
                (82500, 24),
                (157500, 32),
                (200000, 35),
                (500000, 37)
              ],
      qualifiedBrackets =
        \case
          Married ->
            QB.fromPairs
              [ (0, 0),
                (77200, 15),
                (479000, 20)
              ]
          HeadOfHousehold ->
            QB.fromPairs
              [ (0, 0),
                (51700, 15),
                (452400, 20)
              ]
          Single ->
            QB.fromPairs
              [ (0, 0),
                (38600, 15),
                (425800, 20)
              ]
    }
