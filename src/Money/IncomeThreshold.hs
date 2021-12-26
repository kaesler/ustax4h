{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Money.IncomeThreshold where

import Money.Money(Money, mkMoney)

newtype IncomeThreshold = IncomeThreshold Money
  deriving newtype Semigroup
  deriving newtype Monoid
  deriving newtype Show

zero :: IncomeThreshold
zero = IncomeThreshold (mkMoney 0.0)