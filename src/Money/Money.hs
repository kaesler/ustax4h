{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Money.Money
  ( Deduction,
    Income,
    IncomeThreshold,
    TaxableIncome,
    TaxCredit,
    TaxPayable,
    amountAbove,
    applyTaxRate,
  )
where

import Data.Monoid (Sum (Sum))
import TaxRate (TaxRate (toDouble))

type Money = Sum Double

mkMoney :: Double -> Money
mkMoney d
  | d < 0 = error "Money can't be negative"
  | otherwise = Sum d

zero :: Money
zero = mkMoney 0.0

monus :: Money -> Money -> Money
monus (Sum d1) (Sum d2)
  | d1 > d2 = mkMoney (d1 - d2)
  | otherwise = zero

newtype Deduction = Deduction Money
  deriving newtype (Semigroup)
  deriving newtype (Monoid)
  deriving newtype (Show)

newtype Income = Income Money
  deriving newtype (Semigroup)
  deriving newtype (Monoid)
  deriving newtype (Show)

newtype IncomeThreshold = IncomeThreshold Money
  deriving newtype (Semigroup)
  deriving newtype (Monoid)
  deriving newtype (Show)

newtype TaxableIncome = TaxableIncome Money
  deriving newtype (Semigroup)
  deriving newtype (Monoid)
  deriving newtype (Show)

thresholdAsTaxableIncome :: IncomeThreshold -> TaxableIncome
thresholdAsTaxableIncome (IncomeThreshold m) = TaxableIncome m

amountAbove :: TaxableIncome -> IncomeThreshold -> TaxableIncome
amountAbove (TaxableIncome ti) (IncomeThreshold it) = TaxableIncome $ ti `monus` it

newtype TaxCredit = TaxCredit Money
  deriving newtype (Semigroup)
  deriving newtype (Monoid)
  deriving newtype (Show)

newtype TaxPayable = TaxPayable Money
  deriving newtype (Semigroup)
  deriving newtype (Monoid)
  deriving newtype (Show)

applyTaxRate :: TaxRate r => r -> TaxableIncome -> TaxPayable
applyTaxRate r (TaxableIncome (Sum ti)) = TaxPayable (mkMoney $ ti * toDouble r)