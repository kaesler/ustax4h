module Federal.Regime
  ( Regime (..),
  )
where

data Regime = Trump | PreTrump
  deriving (Eq, Ord, Show, Enum)
