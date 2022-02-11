{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Federal.Yearly.Year2021
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
      year = 2021,
      perPersonExemption = makeFromInt undefined ,
      unadjustedStandardDeduction =
        \case
          HeadOfHousehold -> makeFromInt undefined
          Single -> makeFromInt undefined,
      adjustmentWhenOver65 = makeFromInt undefined,
      adjustmentWhenOver65AndSingle = makeFromInt undefined,
      ordinaryBrackets =
        \case
          HeadOfHousehold ->
            OB.fromRPairs
              [ 
              ]
          Single ->
            OB.fromRPairs
              [ 
              ],
      qualifiedBrackets =
        \case
          HeadOfHousehold ->
            QB.fromRPairs
              [ 
              ]
          Single ->
            QB.fromRPairs
              [ 
              ]
    }