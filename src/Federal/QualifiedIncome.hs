module Federal.QualifiedIncome
  ( QualifiedIncomeBrackets,
    QualifiedRate (..),
    applyQualifiedIncomeBrackets,
    fromPairs,
    inflate,
    startOfNonZeroQualifiedRateBracket,
  )
where

import CommonTypes
  ( FilingStatus (..),
    Year,
  )
import Data.Coerce (coerce)
import qualified Data.List as List
import Data.List.NonEmpty as NonEmpty (fromList, reverse, (!!))
import Data.Map.NonEmpty as NEMap (NEMap, assocs, elems, fromList)
import Federal.BracketTypes (BracketStart (..))
import Federal.Types
  ( OrdinaryIncome,
    QualifiedIncome,
  )
import Math (nonNegSub, roundHalfUp)

type QualifiedIncomeBrackets = NEMap QualifiedRate BracketStart

newtype QualifiedRate = QualifiedRate Double
  deriving (Eq, Ord, Show)

qualifiedRateAsFraction :: QualifiedRate -> Double
qualifiedRateAsFraction (QualifiedRate r) = r / 100.0

fromPairs :: [(Double, Int)] -> QualifiedIncomeBrackets
fromPairs = NEMap.fromList . NonEmpty.fromList . fmap f
  where
    f (rateAsDouble, startAsInt) = (QualifiedRate rateAsDouble, BracketStart startAsInt)

inflate :: QualifiedIncomeBrackets -> Double -> QualifiedIncomeBrackets
inflate brackets factor =
  fmap inflateBracketStart brackets
  where
    inflateBracketStart (BracketStart s) =
      BracketStart $ round $ roundHalfUp $ factor * fromIntegral s

startOfNonZeroQualifiedRateBracket :: QualifiedIncomeBrackets -> Int
startOfNonZeroQualifiedRateBracket brackets =
  -- The start of the 2nd-to-bottom bracket.
  coerce $ (NonEmpty.!!) (NEMap.elems brackets) 1

applyQualifiedIncomeBrackets :: QualifiedIncomeBrackets -> OrdinaryIncome -> QualifiedIncome -> Double
applyQualifiedIncomeBrackets brackets taxableOrdinaryIncome qualifiedIncome =
  let bracketsDescending = NonEmpty.reverse (NEMap.assocs brackets)
   in third (List.foldl func (0.0, qualifiedIncome, 0.0) bracketsDescending)
  where
    totalTaxableIncome = taxableOrdinaryIncome + qualifiedIncome
    third :: (a, b, c) -> c
    third (_, _, a) = a
    func :: (Double, Double, Double) -> (QualifiedRate, BracketStart) -> (Double, Double, Double)
    func (totalIncomeInHigherBrackets, gainsYetToBeTaxed, gainsTaxSoFar) (rate, BracketStart start) =
      let totalIncomeYetToBeTaxed = totalTaxableIncome `nonNegSub` totalIncomeInHigherBrackets
          ordinaryIncomeYetToBeTaxed = totalIncomeYetToBeTaxed `nonNegSub` gainsYetToBeTaxed
          totalIncomeInThisBracket = totalIncomeYetToBeTaxed `nonNegSub` fromIntegral start
          ordinaryIncomeInThisBracket = ordinaryIncomeYetToBeTaxed `nonNegSub` fromIntegral start
          gainsInThisBracket = totalIncomeInThisBracket `nonNegSub` ordinaryIncomeInThisBracket
          taxInThisBracket = gainsInThisBracket * qualifiedRateAsFraction rate
       in ( totalIncomeInHigherBrackets + totalIncomeInThisBracket,
            gainsYetToBeTaxed `nonNegSub` gainsInThisBracket,
            gainsTaxSoFar + taxInThisBracket
          )
