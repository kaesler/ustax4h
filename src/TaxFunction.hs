{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module TaxFunction()

where

import Money.TaxableIncome ( TaxableIncome )
import Money.TaxPayable ( TaxPayable )
import TaxRate (TaxRate)
import Money.IncomeThreshold ( IncomeThreshold )
import Brackets (Brackets)

newtype TaxFunction = TaxFunction (TaxableIncome -> TaxPayable)
  deriving newtype Semigroup
  deriving newtype Monoid

thresholdTaxFunction :: TaxRate r => IncomeThreshold -> r -> TaxFunction
thresholdTaxFunction = undefined

flatTaxFunction :: TaxRate r => r -> TaxFunction
flatTaxFunction = undefined

bracketsTaxFunction :: TaxRate r =>  Brackets r -> TaxFunction
bracketsTaxFunction = undefined
