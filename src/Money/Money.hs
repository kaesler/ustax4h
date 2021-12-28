{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Money.Money
  ( Deduction,
    Income,
    IncomeThreshold,
    TaxableIncome,
    TaxCredit,
    TaxPayable,
  )
where

import Data.Monoid (Sum (Sum))

type Money = Sum Double

mkMoney :: Double -> Money
mkMoney = Sum

monus :: Money -> Money -> Money
monus (Sum d1) (Sum d2)
  | d1 > d2 = mkMoney (d1 - d2)
  | otherwise = mkMoney 0.0

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

newtype TaxCredit = TaxCredit Money
  deriving newtype (Semigroup)
  deriving newtype (Monoid)
  deriving newtype (Show)

newtype TaxPayable = TaxPayable Money
  deriving newtype (Semigroup)
  deriving newtype (Monoid)
  deriving newtype (Show)