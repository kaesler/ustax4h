{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Federal.Yearly.Year2016
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
import Federal.Yearly.YearlyValues (YearlyValues (..))
import Moneys (makeFromInt)

values :: YearlyValues
values =
  YearlyValues
    { regime = PreTrump,
      year = 2016,
      perPersonExemption = makeFromInt 4050,
      unadjustedStandardDeduction =
        \case
          HeadOfHousehold -> makeFromInt 9200
          Single -> makeFromInt 6300,
      adjustmentWhenOver65 = makeFromInt 1250,
      adjustmentWhenOver65AndSingle = makeFromInt 300,
      ordinaryBrackets =
        \case
          HeadOfHousehold ->
            OB.fromRPairs
              [ (0, 10),
                (13250, 15),
                (50400, 25),
                (130150, 28),
                (210800, 33),
                (413350, 35),
                (441000, 39.6)
              ]
          Single ->
            OB.fromRPairs
              [ (0, 10),
                (9275, 15),
                (37650, 25),
                (91150, 28),
                (190150, 33),
                (413350, 35),
                (415050, 39.6)
              ],
      qualifiedBrackets =
        \case
          HeadOfHousehold ->
            QB.fromRPairs
              [ (0, 0),
                (50400, 5),
                (441000, 20)
              ]
          Single ->
            QB.fromRPairs
              [ (0, 0),
                (37650, 15),
                (415050, 20)
              ]
    }