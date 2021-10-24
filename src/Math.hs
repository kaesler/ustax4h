module Math
  ( nonNeg,
    nonNegSub,
    roundHalfUp,
  )
where

nonNeg :: Double -> Double
nonNeg x
  | x < 0.0 = 0.0
  | otherwise = x

nonNegSub :: Double -> Double -> Double
nonNegSub x y = nonNeg (x - y)

roundHalfUp :: Double -> Double
roundHalfUp x =
  let xAbs = abs x
      sign = if x >= 0.0 then 1.0 else (-1.0)
      (whole, frac) = properFraction xAbs
   in (sign *) $ fromInteger $ if frac >= 0.5 then whole + 1 else whole
