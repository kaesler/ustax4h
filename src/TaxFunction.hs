{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TaxFunction () where

import Brackets (Brackets)
import Money.Money (Income, IncomeThreshold, TaxPayable, TaxableIncome)
import TaxRate (TaxRate)

newtype TaxFunction = TaxFunction (TaxableIncome -> TaxPayable)
  deriving newtype (Semigroup)
  deriving newtype (Monoid)

thresholdTaxFunction :: TaxRate r => IncomeThreshold -> r -> TaxFunction
thresholdTaxFunction = undefined

flatTaxFunction :: TaxRate r => r -> TaxFunction
flatTaxFunction = undefined

bracketsTaxFunction :: TaxRate r => Brackets r -> TaxFunction
bracketsTaxFunction = undefined
