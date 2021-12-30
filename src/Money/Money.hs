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
    inflateThreshold,
    mkIncomeThreshold,
    thresholdDifference,
  )
where

import Data.Monoid (Sum (Sum))
import Math (roundHalfUp)
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

diff :: Money -> Money -> Money
diff (Sum m1) (Sum m2) = mkMoney (abs (m1 - m2))

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

mkIncomeThreshold :: Integer -> IncomeThreshold
mkIncomeThreshold i = IncomeThreshold $ mkMoney (fromIntegral i)

thresholdDifference :: IncomeThreshold -> IncomeThreshold -> TaxableIncome
thresholdDifference (IncomeThreshold m1) (IncomeThreshold m2) = TaxableIncome (diff m1 m2)

inflateThreshold :: Double -> IncomeThreshold -> IncomeThreshold
inflateThreshold factor (IncomeThreshold (Sum m)) = IncomeThreshold $ Sum $ roundHalfUp (m * factor)

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