module TaxRate( 
  TaxRate(..) 
  )

where

class TaxRate r where
  zero :: r
  toDouble :: r -> Double
  absoluteDifference :: r -> r -> r
