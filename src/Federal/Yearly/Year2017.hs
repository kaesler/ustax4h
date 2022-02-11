{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Federal.Yearly.Year2017
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
    { regime = PreTrump,
      year = 2017,
      perPersonExemption = makeFromInt 4050,
      unadjustedStandardDeduction =
        \case
          HeadOfHousehold -> makeFromInt 9350
          Single -> makeFromInt 6350,
      adjustmentWhenOver65 = makeFromInt 1250,
      adjustmentWhenOver65AndSingle = makeFromInt 300,
      ordinaryBrackets =
        \case
          HeadOfHousehold ->
            OB.fromRPairs
              [ (0, 10),
                (13350, 15),
                (50800, 25),
                (131200, 28),
                (212500, 33),
                (416700, 35),
                (444550, 39.6)
              ]
          Single ->
            OB.fromRPairs
              [ (0, 10),
                (9325, 15),
                (37950, 25),
                (91900, 28),
                (191650, 33),
                (416700, 35),
                (418400, 39.6)
              ],
      qualifiedBrackets =
        \case
          HeadOfHousehold ->
            QB.fromRPairs
              [ (0, 0),
                (50800, 15),
                (444550, 20)
              ]
          Single ->
            QB.fromRPairs
              [ (0, 0),
                (37950, 15),
                (418400, 20)
              ]
    }