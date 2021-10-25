module Federal.OrdinaryIncome
  ( OrdinaryRate (..),
    OrdinaryIncomeBrackets,
    applyOrdinaryIncomeBrackets,
    bottomRateOnOrdinaryIncome,
    incomeToEndOfOrdinaryBracket,
    ordinaryIncomeBracketWidth,
    ordinaryRateAsFraction,
    ordinaryRatesExceptTop,
    taxToEndOfOrdinaryBracket,
    topRateOnOrdinaryIncome,
  )
where

import CommonTypes
import Data.Coerce (coerce)
import qualified Data.List as List
import Data.List.NonEmpty as NonEmpty (fromList, head, last, reverse, tail, takeWhile, toList)
import Data.Map.NonEmpty as NEMap (NEMap, assocs, fromList, keys, lookup)
import Data.Maybe (fromJust)
import Federal.BracketTypes
import Federal.Deductions
import Math

type OrdinaryIncomeBrackets = NEMap OrdinaryRate BracketStart

newtype OrdinaryRate = OrdinaryRate Double
  deriving (Eq, Ord, Show)

ordinaryRateAsFraction :: OrdinaryRate -> Double
ordinaryRateAsFraction (OrdinaryRate r) = r / 100.0

applyOrdinaryIncomeBrackets :: Year -> FilingStatus -> OrdinaryIncome -> Double
applyOrdinaryIncomeBrackets year fs taxableOrdinaryincome =
  let bracketsDescending = NonEmpty.reverse (NEMap.assocs (ordinaryBracketStarts year fs))
   in snd (List.foldl func (taxableOrdinaryincome, 0.0) bracketsDescending)
  where
    func :: (Double, Double) -> (OrdinaryRate, BracketStart) -> (Double, Double)
    func (incomeYetToBeTaxed, taxSoFar) (rate, BracketStart start) =
      let incomeInThisBracket = incomeYetToBeTaxed `nonNegSub` fromInteger start
          taxInThisBracket = incomeInThisBracket * ordinaryRateAsFraction rate
       in ( incomeYetToBeTaxed `nonNegSub` incomeInThisBracket,
            taxSoFar + taxInThisBracket
          )

ordinaryBracketStarts :: Year -> FilingStatus -> NEMap OrdinaryRate BracketStart
ordinaryBracketStarts _ Single =
  NEMap.fromList $
    NonEmpty.fromList
      [ (OrdinaryRate 10, BracketStart 0),
        (OrdinaryRate 12, BracketStart 9950),
        (OrdinaryRate 22, BracketStart 40525),
        (OrdinaryRate 24, BracketStart 86375),
        (OrdinaryRate 32, BracketStart 164925),
        (OrdinaryRate 35, BracketStart 209425),
        (OrdinaryRate 37, BracketStart 523600)
      ]
ordinaryBracketStarts _ HeadOfHousehold =
  NEMap.fromList $
    NonEmpty.fromList
      [ (OrdinaryRate 10, BracketStart 0),
        (OrdinaryRate 12, BracketStart 14200),
        (OrdinaryRate 22, BracketStart 54200),
        (OrdinaryRate 24, BracketStart 86350),
        (OrdinaryRate 32, BracketStart 164900),
        (OrdinaryRate 35, BracketStart 209400),
        (OrdinaryRate 37, BracketStart 523600)
      ]

ordinaryRateSuccessor :: Year -> FilingStatus -> OrdinaryRate -> Maybe OrdinaryRate
ordinaryRateSuccessor year fs rate =
  do
    let brackets = ordinaryBracketStarts year fs
    let rates = NEMap.keys brackets
    let pairs = Prelude.zip (toList rates) (NonEmpty.tail rates)
    pair <- List.find (\p -> fst p == rate) pairs
    Just (snd pair)

ordinaryRatesExceptTop :: Year -> FilingStatus -> [OrdinaryRate]
ordinaryRatesExceptTop year fs =
  let brackets = ordinaryBracketStarts year fs
      rates = NEMap.keys brackets
      topRate = NonEmpty.last rates
   in NonEmpty.takeWhile (/= topRate) rates

incomeToEndOfOrdinaryBracket :: Year -> FilingStatus -> OrdinaryRate -> Double
incomeToEndOfOrdinaryBracket year filingStatus bracketRate =
  let bracketStarts = ordinaryBracketStarts year filingStatus
      successorRate = fromJust (ordinaryRateSuccessor year filingStatus bracketRate)
      BracketStart startOfSuccessor = fromJust (NEMap.lookup successorRate bracketStarts)
      StandardDeduction deduction = standardDeduction year filingStatus
   in fromInteger (startOfSuccessor + deduction)

taxToEndOfOrdinaryBracket :: Year -> FilingStatus -> OrdinaryRate -> Double
taxToEndOfOrdinaryBracket year filingStatus bracketRate =
  let relevantRates = List.takeWhile (<= bracketRate) (ordinaryRatesExceptTop year filingStatus)
      bracketWidths = List.map (ordinaryIncomeBracketWidth year filingStatus) relevantRates
      pairs = relevantRates `zip` bracketWidths
      taxesDue = List.map taxForBracket pairs
        where
          taxForBracket (rate, width) =
            fromIntegral width * ordinaryRateAsFraction rate
   in List.sum taxesDue

bottomRateOnOrdinaryIncome :: Year -> FilingStatus -> OrdinaryRate
bottomRateOnOrdinaryIncome year fs = NonEmpty.head $ NEMap.keys $ ordinaryBracketStarts year fs

topRateOnOrdinaryIncome :: Year -> FilingStatus -> OrdinaryRate
topRateOnOrdinaryIncome year fs = NonEmpty.last $ NEMap.keys $ ordinaryBracketStarts year fs

ordinaryIncomeBracketWidth :: Year -> FilingStatus -> OrdinaryRate -> Integer
ordinaryIncomeBracketWidth year fs rate =
  fromJust
    ( do
        let brackets = ordinaryBracketStarts year fs
        let rates = NEMap.keys brackets
        let pairs = Prelude.zip (toList rates) (NonEmpty.tail rates)
        pair <- List.find (\p -> fst p == rate) pairs
        let successor = snd pair
        rateStart <- NEMap.lookup rate brackets
        successorStart <- NEMap.lookup successor brackets
        Just (coerce successorStart - coerce rateStart)
    )
