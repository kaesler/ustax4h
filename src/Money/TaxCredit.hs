{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Money.TaxCredit where

import Money.Money(Money, mkMoney)

newtype TaxCredit = TaxCredit Money
  deriving newtype Semigroup
  deriving newtype Monoid
  deriving newtype Show

zero :: TaxCredit
zero = TaxCredit (mkMoney 0.0)