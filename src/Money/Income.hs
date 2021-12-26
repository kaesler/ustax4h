{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Money.Income where

import Money.Money(Money, mkMoney)

newtype Income = Income Money
  deriving newtype Semigroup
  deriving newtype Monoid
  deriving newtype Show

zero :: Income
zero = Income (mkMoney 0.0)