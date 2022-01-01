module Federal.BracketTypes
  ( BracketStart (..),
  )
where

newtype BracketStart = BracketStart Int
  deriving (Eq, Ord, Show)
