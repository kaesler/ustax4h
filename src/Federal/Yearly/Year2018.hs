{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Federal.Yearly.Year2018
  ( values,
  )
where

import CommonTypes (FilingStatus (HeadOfHousehold, Single))
import Federal.OrdinaryBrackets as OB
  ( fromRPairs,
  )
import Federal.QualifiedBrackets as QB
  ( fromRPairs,
  )
import Federal.Regime (Regime (..))
import Federal.Yearly.Type (YearlyValues (..))
import Moneys (makeFromInt)

values :: YearlyValues
values =
  YearlyValues
    { regime = Trump,
      year = 2018,
      perPersonExemption = makeFromInt 0,
      unadjustedStandardDeduction =
        \case
          HeadOfHousehold -> makeFromInt 18000
          Single -> makeFromInt 12000,
      adjustmentWhenOver65 = makeFromInt 1300,
      adjustmentWhenOver65AndSingle = makeFromInt 300,
      ordinaryBrackets =
        \case
          HeadOfHousehold ->
            OB.fromRPairs
              [ (0, 10),
                (13600, 12),
                (51800, 22),
                (82500, 24),
                (157500, 32),
                (200000, 35),
                (500000, 37)
              ]
          Single ->
            OB.fromRPairs
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
          HeadOfHousehold ->
            QB.fromRPairs
              [ (0, 0),
                (51700, 15),
                (452400, 20)
              ]
          Single ->
            QB.fromRPairs
              [ (0, 0),
                (38600, 15),
                (425800, 20)
              ]
    }
