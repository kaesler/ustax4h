{-# LANGUAGE DerivingStrategies #-}

module TaxFunction () where

import Brackets (Brackets)
import Money.Money (Income, IncomeThreshold, TaxPayable, TaxableIncome, amountAbove, applyTaxRate)
import TaxRate (TaxRate (zero))

type TaxFunction = TaxableIncome -> TaxPayable

thresholdTaxFunction :: TaxRate r => IncomeThreshold -> r -> TaxFunction
thresholdTaxFunction threshold rate ti = applyTaxRate rate (amountAbove ti threshold)

flatTaxFunction :: TaxRate r => r -> TaxFunction
flatTaxFunction = thresholdTaxFunction zero
  where
    zero = mempty

bracketsTaxFunction :: TaxRate r => Brackets r -> TaxFunction
bracketsTaxFunction = undefined
