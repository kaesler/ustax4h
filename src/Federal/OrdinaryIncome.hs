module Federal.OrdinaryIncome
  ( OrdinaryRate (..),
    OrdinaryIncomeBrackets,
    applyOrdinaryIncomeBrackets,
    bottomRateOnOrdinaryIncome,
    fromPairs,
    incomeToEndOfOrdinaryBracket,
    ordinaryIncomeBracketsFor,
    ordinaryRateAsFraction,
    ordinaryRatesExceptTop,
    topRateOnOrdinaryIncome,
    taxToEndOfOrdinaryBracket,
  )
where

import CommonTypes
  ( FilingStatus (..),
    Money,
    OrdinaryIncome,
    Year,
  )
import Data.Coerce (coerce)
import qualified Data.List as List
import Data.List.NonEmpty as NonEmpty (fromList, head, last, reverse, tail, takeWhile, toList)
import Data.Map.NonEmpty as NEMap (NEMap, assocs, fromList, keys, lookup)
import Data.Maybe (fromJust)
import Federal.BracketTypes (BracketStart (..))
import Federal.Deductions (StandardDeduction (..))
import Math (nonNegSub)

type OrdinaryIncomeBrackets = NEMap OrdinaryRate BracketStart

newtype OrdinaryRate = OrdinaryRate Double
  deriving (Eq, Ord, Show)

ordinaryRateAsFraction :: OrdinaryRate -> Double
ordinaryRateAsFraction (OrdinaryRate r) = r / 100.0

applyOrdinaryIncomeBrackets :: OrdinaryIncomeBrackets -> OrdinaryIncome -> Money
applyOrdinaryIncomeBrackets brackets taxableOrdinaryincome =
  let bracketsDescending = NonEmpty.reverse (NEMap.assocs brackets)
   in snd (List.foldl func (taxableOrdinaryincome, 0.0) bracketsDescending)
  where
    func :: (Double, Double) -> (OrdinaryRate, BracketStart) -> (Double, Double)
    func (incomeYetToBeTaxed, taxSoFar) (rate, BracketStart start) =
      let incomeInThisBracket = incomeYetToBeTaxed `nonNegSub` fromInteger start
          taxInThisBracket = incomeInThisBracket * ordinaryRateAsFraction rate
       in ( incomeYetToBeTaxed `nonNegSub` incomeInThisBracket,
            taxSoFar + taxInThisBracket
          )

fromPairs :: [(OrdinaryRate, BracketStart)] -> OrdinaryIncomeBrackets
fromPairs = NEMap.fromList . NonEmpty.fromList

-- TODO: delete
ordinaryIncomeBracketsFor :: Year -> FilingStatus -> OrdinaryIncomeBrackets
ordinaryIncomeBracketsFor _ Single =
  fromPairs
    [ (OrdinaryRate 10, BracketStart 0),
      (OrdinaryRate 12, BracketStart 9950),
      (OrdinaryRate 22, BracketStart 40525),
      (OrdinaryRate 24, BracketStart 86375),
      (OrdinaryRate 32, BracketStart 164925),
      (OrdinaryRate 35, BracketStart 209425),
      (OrdinaryRate 37, BracketStart 523600)
    ]

ordinaryIncomeBracketsFor _ HeadOfHousehold =
  fromPairs
    [ (OrdinaryRate 10, BracketStart 0),
      (OrdinaryRate 12, BracketStart 14200),
      (OrdinaryRate 22, BracketStart 54200),
      (OrdinaryRate 24, BracketStart 86350),
      (OrdinaryRate 32, BracketStart 164900),
      (OrdinaryRate 35, BracketStart 209400),
      (OrdinaryRate 37, BracketStart 523600)
    ]

rateSuccessor :: OrdinaryIncomeBrackets -> OrdinaryRate -> Maybe OrdinaryRate
rateSuccessor brackets rate =
  do
    let rates = NEMap.keys brackets
    let pairs = Prelude.zip (toList rates) (NonEmpty.tail rates)
    pair <- List.find (\p -> fst p == rate) pairs
    Just (snd pair)

ordinaryRatesExceptTop :: OrdinaryIncomeBrackets -> [OrdinaryRate]
ordinaryRatesExceptTop brackets =
  let rates = NEMap.keys brackets
      topRate = NonEmpty.last rates
   in NonEmpty.takeWhile (/= topRate) rates

incomeToEndOfOrdinaryBracket :: OrdinaryIncomeBrackets -> StandardDeduction -> OrdinaryRate -> Double
incomeToEndOfOrdinaryBracket brackets stdDeduction bracketRate =
  let successorRate = fromJust (rateSuccessor brackets bracketRate)
      BracketStart startOfSuccessor = fromJust (NEMap.lookup successorRate brackets)
      StandardDeduction deduction = stdDeduction
   in fromInteger (startOfSuccessor + deduction)

taxToEndOfOrdinaryBracket :: OrdinaryIncomeBrackets -> OrdinaryRate -> Double
taxToEndOfOrdinaryBracket brackets bracketRate =
  let relevantRates = List.takeWhile (<= bracketRate) (ordinaryRatesExceptTop brackets)
      bracketWidths = List.map (ordinaryIncomeBracketWidth brackets) relevantRates
      pairs = relevantRates `zip` bracketWidths
      taxesDue = List.map taxForBracket pairs
        where
          taxForBracket (rate, width) =
            fromIntegral width * ordinaryRateAsFraction rate
   in List.sum taxesDue

bottomRateOnOrdinaryIncome :: OrdinaryIncomeBrackets -> OrdinaryRate
bottomRateOnOrdinaryIncome brackets = NonEmpty.head $ NEMap.keys brackets

topRateOnOrdinaryIncome :: OrdinaryIncomeBrackets -> OrdinaryRate
topRateOnOrdinaryIncome brackets = NonEmpty.last $ NEMap.keys brackets

ordinaryIncomeBracketWidth :: OrdinaryIncomeBrackets -> OrdinaryRate -> Integer
ordinaryIncomeBracketWidth brackets rate =
  fromJust
    ( do
        let rates = NEMap.keys brackets
        let pairs = Prelude.zip (toList rates) (NonEmpty.tail rates)
        pair <- List.find (\p -> fst p == rate) pairs
        let successor = snd pair
        rateStart <- NEMap.lookup rate brackets
        successorStart <- NEMap.lookup successor brackets
        Just (coerce successorStart - coerce rateStart)
    )
