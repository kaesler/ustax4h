module MathInSpecs
  ( closeEnoughTo,
  )
where

closeEnoughTo :: Double -> Double -> Bool
closeEnoughTo x y = abs (x - y) <= 1.0
