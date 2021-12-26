{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Money.Moneys (

)

where 

import Data.Monoid

newtype Money = Money (Sum Double)
  deriving newtype Semigroup
  deriving newtype Monoid
instance Show Money where
  show (Money (Sum d)) = show d

mkMoney :: Double -> Money
mkMoney d = Money (Sum d)

monus :: Money -> Money -> Money
monus (Money (Sum d1)) (Money (Sum d2))
  | (d1 > d2) = mkMoney (d1 - d2)
  | otherwise = mkMoney(0.0)

newtype Deduction = Deduction Money
  deriving newtype Semigroup
  deriving newtype Monoid
  deriving newtype Show

newtype Income = Income Money
  deriving newtype Semigroup
  deriving newtype Monoid
  deriving newtype Show

newtype TaxableIncome = TaxableIncome Money
  deriving newtype Semigroup
  deriving newtype Monoid
  deriving newtype Show

newtype IncomeThreshold = IncomeThreshold Money
  deriving newtype Semigroup
  deriving newtype Monoid
  deriving newtype Show

newtype TaxPayable = TaxPayable Money
  deriving newtype Semigroup
  deriving newtype Monoid
  deriving newtype Show

newtype TaxCredit = TaxCredit Money
  deriving newtype Semigroup
  deriving newtype Monoid
  deriving newtype Show

