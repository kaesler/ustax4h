{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Money.Deduction where

import Money.Money(Money, mkMoney)

newtype Deduction = Deduction Money
  deriving newtype Semigroup
  deriving newtype Monoid
  deriving newtype Show

zero :: Deduction
zero = Deduction (mkMoney 0.0)

fromInt :: Int -> Deduction
fromInt i = Deduction $ mkMoney $ fromIntegral i