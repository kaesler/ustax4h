module Federal.QualifiedIncome
  ( QualifiedIncomeBrackets,
    QualifiedRate,
    applyQualifiedIncomeBrackets,
    startOfNonZeroQualifiedRateBracket,
  )
where

import CommonTypes
  ( FilingStatus (..),
    OrdinaryIncome,
    QualifiedIncome
  )
import Data.Coerce (coerce)
import qualified Data.List as List
import Data.List.NonEmpty as NonEmpty (fromList, reverse, (!!))
import Data.Map.NonEmpty as NEMap (NEMap, assocs, elems, fromList)
import Federal.BracketTypes (BracketStart (..))
import Math (nonNegSub)

type QualifiedIncomeBrackets = NEMap QualifiedRate BracketStart
newtype QualifiedRate = QualifiedRate Double
  deriving (Eq, Ord, Show)

qualifiedRateAsFraction :: QualifiedRate -> Double
qualifiedRateAsFraction (QualifiedRate r) = r / 100.0

qualifiedBracketStarts :: FilingStatus -> NEMap QualifiedRate BracketStart
qualifiedBracketStarts Single =
  NEMap.fromList $
    NonEmpty.fromList
      [ (QualifiedRate 0, BracketStart 0),
        (QualifiedRate 15, BracketStart 40400),
        (QualifiedRate 20, BracketStart 445850)
      ]
qualifiedBracketStarts HeadOfHousehold =
  NEMap.fromList $
    NonEmpty.fromList
      [ (QualifiedRate 0, BracketStart 0),
        (QualifiedRate 15, BracketStart 54100),
        (QualifiedRate 20, BracketStart 473850)
      ]

startOfNonZeroQualifiedRateBracket :: FilingStatus -> Integer
startOfNonZeroQualifiedRateBracket fs =
  -- The start of the 2nd-to-bottom bracket.
  coerce $ (NonEmpty.!!) (NEMap.elems (qualifiedBracketStarts fs)) 1

applyQualifiedIncomeBrackets :: FilingStatus -> OrdinaryIncome -> QualifiedIncome -> Double
applyQualifiedIncomeBrackets fs taxableOrdinaryIncome qualifiedIncome =
  let bracketsDescending = NonEmpty.reverse (NEMap.assocs (qualifiedBracketStarts fs))
   in third (List.foldl func (0.0, qualifiedIncome, 0.0) bracketsDescending)
  where
    totalTaxableIncome = taxableOrdinaryIncome + qualifiedIncome
    third :: (a, b, c) -> c
    third (_, _, a) = a
    func :: (Double, Double, Double) -> (QualifiedRate, BracketStart) -> (Double, Double, Double)
    func (totalIncomeInHigherBrackets, gainsYetToBeTaxed, gainsTaxSoFar) (rate, BracketStart start) =
      let totalIncomeYetToBeTaxed = totalTaxableIncome `nonNegSub` totalIncomeInHigherBrackets
          ordinaryIncomeYetToBeTaxed = totalIncomeYetToBeTaxed `nonNegSub` gainsYetToBeTaxed
          totalIncomeInThisBracket = totalIncomeYetToBeTaxed `nonNegSub` fromInteger start
          ordinaryIncomeInThisBracket = ordinaryIncomeYetToBeTaxed `nonNegSub` fromInteger start
          gainsInThisBracket = totalIncomeInThisBracket `nonNegSub` ordinaryIncomeInThisBracket
          taxInThisBracket = gainsInThisBracket * qualifiedRateAsFraction rate
       in ( totalIncomeInHigherBrackets + totalIncomeInThisBracket,
            gainsYetToBeTaxed `nonNegSub` gainsInThisBracket,
            gainsTaxSoFar + taxInThisBracket
          )
