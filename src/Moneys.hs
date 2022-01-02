{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Moneys
  ( Deduction,
    Income (..), -- TODO: hide ctors
    IncomeThreshold,
    ItemizedDeductions,
    OrdinaryIncome,
    QualifiedIncome,
    SocSec,
    TaxableIncome,
    TaxCredit,
    TaxPayable,
    amountAbove,
    applyDeductions,
    applyTaxRate,
    hackIncomeFromDouble,
    hackTaxPayableToDouble,
    inflateThreshold,
    mkDeduction,
    mkIncomeThreshold,
    roundTaxPayable,
    thresholdDifference,
    thresholdAsTaxableIncome,
  )
where

import Data.Monoid (Sum (Sum))
import GHC.Base (coerce)
import Math (roundHalfUp)
import TaxRate (TaxRate (toDouble))

type Money = Sum Double

type ItemizedDeductions = Deduction

type OrdinaryIncome = Income

type QualifiedIncome = Income

type SocSec = Income

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

mkDeduction :: Int -> Deduction
mkDeduction i = coerce $ mkMoney $ fromIntegral i

newtype Income = Income Money
  deriving newtype (Semigroup)
  deriving newtype (Monoid)
  deriving newtype (Show)

-- TODO: temporary
hackIncomeFromDouble :: Double -> Income
hackIncomeFromDouble = coerce

newtype IncomeThreshold = IncomeThreshold Money
  deriving newtype (Semigroup)
  deriving newtype (Monoid)
  deriving newtype (Show)

mkIncomeThreshold :: Int -> IncomeThreshold
mkIncomeThreshold i = coerce (fromIntegral i :: Double)

thresholdDifference :: IncomeThreshold -> IncomeThreshold -> TaxableIncome
thresholdDifference it1 it2 = coerce $ diff (coerce it1) (coerce it2)

inflateThreshold :: Double -> IncomeThreshold -> IncomeThreshold
inflateThreshold factor threshold = coerce $ roundHalfUp (coerce threshold * factor)

newtype TaxableIncome = TaxableIncome Money
  deriving newtype (Semigroup)
  deriving newtype (Monoid)
  deriving newtype (Show)

hackTaxPayableToDouble :: TaxPayable -> Double
hackTaxPayableToDouble = coerce

roundTaxPayable :: TaxPayable -> TaxPayable
roundTaxPayable tp = coerce $ roundHalfUp $ coerce tp

thresholdAsTaxableIncome :: IncomeThreshold -> TaxableIncome
thresholdAsTaxableIncome = coerce

amountAbove :: TaxableIncome -> IncomeThreshold -> TaxableIncome
amountAbove income threshold = coerce $ coerce income `monus` coerce threshold

applyDeductions :: Income -> Deduction -> TaxableIncome
applyDeductions income deductions = coerce $ coerce income `monus` coerce deductions

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
