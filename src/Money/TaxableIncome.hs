{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Money.TaxableIncome where

import Money.Money(Money, mkMoney, monus)
import Money.Income ( Income(..) )
import Money.Deduction ( Deduction(..) )

newtype TaxableIncome = TaxableIncome Money
  deriving newtype Semigroup
  deriving newtype Monoid
  deriving newtype Show

zero :: TaxableIncome
zero = TaxableIncome (mkMoney 0.0)

applyDeductions :: Income -> [Deduction] -> TaxableIncome
applyDeductions (Income mi) deductions = 
  let (Deduction ds) = mconcat deductions
   in TaxableIncome $ mi `monus` ds

