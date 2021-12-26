{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Money.TaxPayable where

import Money.Money(Money, mkMoney)

newtype TaxPayable = TaxPayable Money
  deriving newtype Semigroup
  deriving newtype Monoid
  deriving newtype Show

zero :: TaxPayable
zero = TaxPayable (mkMoney 0.0)