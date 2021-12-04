module Federal.Regime
  ( Regime (..),
    lastYearKnown,
    requireRegimeValidInYear,
  )
where

import CommonTypes (Year)
import Federal.BracketTypes ()
import GHC.Stack (HasCallStack)
import Text.Printf (printf)

data Regime = Trump | PreTrump
  deriving (Eq, Ord, Show, Enum)

lastYearKnown :: Regime -> Year
lastYearKnown Trump = 2022
lastYearKnown PreTrump = 2017

requireRegimeValidInYear :: HasCallStack => Regime -> Year -> ()
requireRegimeValidInYear r y =
  if regimeValidInYear r y
    then ()
    else error $ printf "Regime %s not valid in year %d" (show r) y

regimeValidInYear :: Regime -> Year -> Bool
regimeValidInYear Trump y = y >= 2018
regimeValidInYear PreTrump y = y < 2018 || y > 2025
