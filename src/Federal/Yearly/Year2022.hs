{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Federal.Yearly.Year2022
  ( values,
  )
where

import CommonTypes (FilingStatus (HeadOfHousehold, Single))
import Federal.BoundRegime (BoundRegime)
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
      year = 2022,
      perPersonExemption = makeFromInt 0,
      unadjustedStandardDeduction =
        \case
          HeadOfHousehold -> makeFromInt 19400
          Single -> makeFromInt 12950,
      adjustmentWhenOver65 = makeFromInt 1400,
      adjustmentWhenOver65AndSingle = makeFromInt 350,
      ordinaryBrackets =
        \case
          HeadOfHousehold ->
            OB.fromRPairs
              [ (0, 10),
                (14650, 12),
                (55900, 22),
                (89050, 24),
                (170050, 32),
                (215950, 35),
                (539900, 37)
              ]
          Single ->
            OB.fromRPairs
              [ (0, 10),
                (10275, 12),
                (41775, 22),
                (89075, 24),
                (170050, 32),
                (215950, 35),
                (539900, 37)
              ],
      qualifiedBrackets =
        \case
          HeadOfHousehold ->
            QB.fromRPairs
              [ (0, 0),
                (55800, 15),
                (488500, 20)
              ]
          Single ->
            QB.fromRPairs
              [ (0, 0),
                (41675, 15),
                (459750, 20)
              ]
    }