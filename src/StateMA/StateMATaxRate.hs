{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module StateMA.StateMATaxRate(
  StateMATaxRate,
  mkStateMATaxRate
)

where

import Text.Printf (printf)
import TaxRate ( TaxRate(..) )

newtype StateMATaxRate = StateMATaxRate Double 
  deriving newtype (Eq, Ord, Show)

mkStateMATaxRate :: Double -> StateMATaxRate
mkStateMATaxRate d
  | d < 0.0 = error $ printf "Invalid StateMATaxRate %d" d
  | d > 0.9 = error  $ printf "Invalid StateMATaxRate %d" d
  | otherwise = StateMATaxRate d

instance TaxRate StateMATaxRate where
  zeroRate = mkStateMATaxRate 0.0
  toDouble (StateMATaxRate d) = d
  absoluteDifference (StateMATaxRate d1) (StateMATaxRate d2) = 
    StateMATaxRate (abs (d1 - d2))
