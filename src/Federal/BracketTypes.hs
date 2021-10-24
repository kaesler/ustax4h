module Federal.BracketTypes(
  BracketStart(..)
)
where

newtype BracketStart = BracketStart Integer
  deriving (Eq, Ord, Show)