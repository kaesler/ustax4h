{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Federal.Yearly.Year2017
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
    { regime = PreTrump,
      year = 2017,
      perPersonExemption = makeFromInt 4050,
      unadjustedStandardDeduction =
        \case
          Married -> makeFromInt 12700
          HeadOfHousehold -> makeFromInt 9350
          Single -> makeFromInt 6350,
      adjustmentWhenOver65 = makeFromInt 1250,
      adjustmentWhenOver65AndSingle = makeFromInt 300,
      ordinaryBrackets =
        \case
          Married ->
            OB.fromPairs
              [ (0, 10),
                (18650, 15),
                (75900, 25),
                (153100, 28),
                (233350, 33),
                (416700, 35),
                (470700, 39.6)
              ]
          HeadOfHousehold ->
            OB.fromPairs
              [ (0, 10),
                (13350, 15),
                (50800, 25),
                (131200, 28),
                (212500, 33),
                (416700, 35),
                (444550, 39.6)
              ]              
          Single ->
            OB.fromPairs
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
          Married ->
            QB.fromPairs
              [ (0, 0),
                (75900, 15),
                (470700, 20)
              ]
          HeadOfHousehold ->
            QB.fromPairs
              [ (0, 0),
                (50800, 15),
                (444550, 20)
              ]
          Single ->
            QB.fromPairs
              [ (0, 0),
                (37950, 15),
                (418400, 20)
              ]
    }
