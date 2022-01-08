module TaxRate
  ( TaxRate (..),
  )
where

class Ord r => TaxRate r where
  zero :: r
  toDouble :: r -> Double
  absoluteDifference :: r -> r -> r
