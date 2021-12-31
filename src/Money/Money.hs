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
    thresholdAsTaxableIncome,
  )
where

import Data.Monoid (Sum (Sum))
import GHC.Base (coerce)
import Math (roundHalfUp)
import TaxRate (TaxRate (toDouble))

type Money = Sum Double

mkMoney :: Double -> Money
mkMoney d
  | d < 0 = error "Money can't be negative"
  | otherwise = coerce d

zero :: Money
zero = mkMoney 0.0

monus :: Money -> Money -> Money
monus m1 m2
  | m1 > m2 = coerce $ (coerce m1 :: Double) - coerce m2
  | otherwise = zero

diff :: Money -> Money -> Money
diff m1 m2 = coerce $ abs $ m1 - m2

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
mkIncomeThreshold i = coerce (fromIntegral i :: Double)

thresholdDifference :: IncomeThreshold -> IncomeThreshold -> TaxableIncome
thresholdDifference it1 it2 = coerce $ diff (coerce it1) (coerce it2)

inflateThreshold :: Double -> IncomeThreshold -> IncomeThreshold
inflateThreshold factor threshold = coerce $ roundHalfUp (coerce threshold * factor)

newtype TaxableIncome = TaxableIncome Money
  deriving newtype (Semigroup)
  deriving newtype (Monoid)
  deriving newtype (Show)

thresholdAsTaxableIncome :: IncomeThreshold -> TaxableIncome
thresholdAsTaxableIncome = coerce

amountAbove :: TaxableIncome -> IncomeThreshold -> TaxableIncome
amountAbove income threshold = coerce $ coerce income `monus` coerce threshold

newtype TaxCredit = TaxCredit Money
  deriving newtype (Semigroup)
  deriving newtype (Monoid)
  deriving newtype (Show)

newtype TaxPayable = TaxPayable Money
  deriving newtype (Semigroup)
  deriving newtype (Monoid)
  deriving newtype (Show)

applyTaxRate :: TaxRate r => r -> TaxableIncome -> TaxPayable
applyTaxRate rate income = coerce $ coerce income * toDouble rate
