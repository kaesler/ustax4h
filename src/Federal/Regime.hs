module Federal.Regime
  ( Regime (..),
  )
where

data Regime = TCJA | PreTCJA
  deriving (Eq, Ord, Show, Enum)
