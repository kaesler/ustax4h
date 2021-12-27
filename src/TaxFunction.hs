{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module TaxFunction()

where

import Money.TaxableIncome ( TaxableIncome )
import Money.TaxPayable ( TaxPayable )

newtype TaxFunction = TaxFunction (TaxableIncome -> TaxPayable)
  deriving newtype Semigroup
  deriving newtype Monoid