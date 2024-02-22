{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Federal.Yearly.Year2016
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
    { regime = PreTCJA,
      year = 2016,
      perPersonExemption = makeFromInt 4050,
      unadjustedStandardDeduction =
        \case
          MarriedJoint -> makeFromInt 12600
          HeadOfHousehold -> makeFromInt 9300
          Single -> makeFromInt 6300,
      adjustmentWhenOver65 = makeFromInt 1250,
      adjustmentWhenOver65AndSingle = makeFromInt 300,
      ordinaryBrackets =
        \case
          MarriedJoint ->
            OB.fromPairs
              [ (0, 10),
                (18550, 15),
                (75300, 25),
                (151900, 28),
                (231450, 33),
                (413350, 35),
                (466950, 39.6)
              ]
          HeadOfHousehold ->
            OB.fromPairs
              [ (0, 10),
                (13250, 15),
                (50400, 25),
                (130150, 28),
                (210800, 33),
                (413350, 35),
                (441000, 39.6)
              ]
          Single ->
            OB.fromPairs
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
          MarriedJoint ->
            QB.fromPairs
              [ (0, 0),
                (75300, 15),
                (466950, 20)
              ]
          HeadOfHousehold ->
            QB.fromPairs
              [ (0, 0),
                (50400, 15),
                (441000, 20)
              ]
          Single ->
            QB.fromPairs
              [ (0, 0),
                (37650, 15),
                (415050, 20)
              ]
    }
