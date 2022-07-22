{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Moneys
  ( Deduction,
    Income,
    IncomeThreshold,
    TaxableIncome,
    TaxCredit,
    TaxPayable,
    amountOverThreshold,
    applyDeductions,
    applyTaxRate,
    asTaxable,
    closeEnoughTo,
    divInt,
    inflateThreshold,
    isBelow,
    makeFromInt,
    mul,
    noMoney,
    nonZero,
    reduceBy,
    roundTaxPayable,
    taxableAsIncome,
    thresholdDifference,
    thresholdAsTaxableIncome,
    times,
  )
where

import Data.Monoid (Sum (Sum))
import Data.Semigroup (mtimesDefault)
import GHC.Base (Coercible, coerce)
import TaxRate (TaxRate (toDouble))

class Monoid m => HasNoMoney m where
  noMoney :: m
  noMoney = mempty

class Coercible Double h => HasNonZero h where
  nonZero :: h -> Bool
  nonZero h = (coerce h) == (0.0 :: Double)

class Coercible Double h => HasMakeFromInt h where
  makeFromInt :: Int -> h
  makeFromInt i = coerce (fromIntegral i :: Double)

class Coercible Double h => HasMul h where
  mul :: h -> Double -> h
  mul h d = coerce $ d * coerce h

class Coercible Double h => HasCloseEnoughTo h where
  closeEnoughTo :: h -> h -> Bool
  closeEnoughTo x y = abs ((coerce x :: Double) - (coerce y :: Double)) <= 2.0

class Monoid h => HasTimes h where
  -- TODO: requre the Int to be non-negative.
  times :: Int -> h -> h
  times = mtimesDefault

class Coercible Double h => HasAmountOverThreshold h where
  amountOverThreshold :: h -> IncomeThreshold -> h
  amountOverThreshold income threshold = coerce $ monus (coerce income) (coerce threshold)

type Money = Sum Double

mkMoney :: Double -> Money
mkMoney d
  | d < 0 = error "Money can't be negative"
  | otherwise = coerce d

monus :: Money -> Money -> Money
monus m1 m2
  | m1 > m2 = coerce $ (coerce m1 :: Double) - coerce m2
  | otherwise = mkMoney 0.0

diff :: Money -> Money -> Money
diff m1 m2 = coerce $ abs $ m1 - m2

newtype Income = Income Money
  deriving newtype (Eq)
  deriving newtype (Monoid)
  deriving newtype (Ord)
  deriving newtype (Semigroup)
  deriving newtype (Show)

instance HasAmountOverThreshold Income

instance HasMakeFromInt Income

instance HasMul Income

instance HasNoMoney Income

isBelow :: Income -> IncomeThreshold -> Bool
isBelow i it = (coerce i :: Money) < coerce it

asTaxable :: Income -> TaxableIncome
asTaxable = coerce

newtype Deduction = Deduction Money
  deriving newtype (Eq)
  deriving newtype (Monoid)
  deriving newtype (Ord)
  deriving newtype (Semigroup)
  deriving newtype (Show)

instance HasMakeFromInt Deduction

instance HasMul Deduction

instance HasNoMoney Deduction

instance HasTimes Deduction

newtype IncomeThreshold = IncomeThreshold Money
  deriving newtype (Eq)
  deriving newtype (Monoid)
  deriving newtype (Semigroup)
  deriving newtype (Show)

instance HasMakeFromInt IncomeThreshold
instance HasNoMoney IncomeThreshold
instance HasNonZero IncomeThreshold

thresholdDifference :: IncomeThreshold -> IncomeThreshold -> TaxableIncome
thresholdDifference it1 it2 = coerce $ diff (coerce it1) (coerce it2)

inflateThreshold :: Double -> IncomeThreshold -> IncomeThreshold
inflateThreshold factor threshold = coerce $ roundHalfUp (coerce threshold * factor)

thresholdAsTaxableIncome :: IncomeThreshold -> TaxableIncome
thresholdAsTaxableIncome = coerce

newtype TaxableIncome = TaxableIncome Money
  deriving newtype (Eq)
  deriving newtype (Monoid)
  deriving newtype (Ord)
  deriving newtype (Semigroup)
  deriving newtype (Show)

instance HasAmountOverThreshold TaxableIncome

instance HasMakeFromInt TaxableIncome

instance HasNoMoney TaxableIncome

divInt :: TaxableIncome -> Int -> TaxableIncome
divInt ti i = coerce $ (coerce ti :: Double) / fromIntegral i

taxableAsIncome :: TaxableIncome -> Income
taxableAsIncome = coerce

applyDeductions :: Income -> Deduction -> TaxableIncome
applyDeductions income deductions = coerce $ coerce income `monus` coerce deductions

newtype TaxCredit = TaxCredit Money
  deriving newtype (Monoid)
  deriving newtype (Semigroup)
  deriving newtype (Show)

newtype TaxPayable = TaxPayable Money
  deriving newtype (Eq)
  deriving newtype (Monoid)
  deriving newtype (Ord)
  deriving newtype (Semigroup)
  deriving newtype (Show)

instance HasCloseEnoughTo TaxPayable

instance HasMakeFromInt TaxPayable

instance HasNoMoney TaxPayable

applyTaxRate :: TaxRate r => r -> TaxableIncome -> TaxPayable
applyTaxRate rate income = coerce $ coerce income * toDouble rate

reduceBy :: TaxPayable -> TaxPayable -> TaxPayable
reduceBy x y = coerce $ coerce x `monus` coerce y

roundTaxPayable :: TaxPayable -> TaxPayable
roundTaxPayable tp = coerce $ roundHalfUp $ coerce tp

roundHalfUp :: Double -> Double
roundHalfUp x =
  let xAbs = abs x
      sign = if x >= 0.0 then 1.0 else (-1.0)
      (whole, frac) = properFraction xAbs
   in (sign *) $ fromInteger $ if frac >= 0.5 then whole + 1 else whole
