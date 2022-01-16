module TaxRate
  ( TaxRate (..),
  )

where

class Ord r => TaxRate r where
  zeroRate :: r
  toDouble :: r -> Double
  absoluteDifference :: r -> r -> r
