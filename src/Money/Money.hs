{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Money.Money (
  Money,
  mkMoney,
  monus
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
  | d1 > d2 = mkMoney (d1 - d2)
  | otherwise = mkMoney 0.0

