module Taxes
  ( Age (..),
    FilingStatus (..),
    OrdinaryRate (..),
    QualifiedRate (..),
    QualifiedInvestmentIncome,
    SocialSecurityBenefits (..),
    SSRelevantIncome (..),
    StandardDeduction (..),
    TaxableOrdinaryIncome (..),
    applyOrdinaryIncomeBrackets,
    applyQualifiedBrackets,
    bottomRateOnOrdinaryIncome,
    bracketWidth,
    ordinaryRateAsFraction,
    rmdFractionForAge,
    standardDeduction,
    taxableSocialSecurity,
    topRateOnOrdinaryIncome,
  )
where

import Data.Coerce (coerce)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)

type Year = Integer

newtype Age = Age Integer
  deriving (Eq, Ord, Show)

data FilingStatus = HeadOfHousehold | Single
  deriving (Eq, Ord, Show, Enum)

newtype OrdinaryRate = OrdinaryRate Integer
  deriving (Eq, Ord, Show)

ordinaryRateAsFraction :: OrdinaryRate -> Double
ordinaryRateAsFraction (OrdinaryRate r) = fromIntegral r / 100.0

newtype QualifiedRate = QualifiedRate Integer
  deriving (Eq, Ord, Show)

qualifiedRateAsFraction :: QualifiedRate -> Double
qualifiedRateAsFraction (QualifiedRate r) = fromIntegral r / 100.0

newtype BracketStart = BracketStart Integer
  deriving (Eq, Ord, Show)

newtype StandardDeduction = StandardDeduction Integer
  deriving (Eq, Ord, Show)

type SocialSecurityBenefits = Double

type SSRelevantIncome = Double

type CombinedIncome = Double

type OrdinaryBracketStarts = Map.Map OrdinaryRate BracketStart

type QualifiedBracketStarts = Map.Map QualifiedRate BracketStart

type TaxableOrdinaryIncome = Double

type QualifiedInvestmentIncome = Double

type DistributionPeriod = Double

nonNeg :: Double -> Double
nonNeg x
  | x < 0.0 = 0.0
  | otherwise = x

ordinaryBracketStarts :: FilingStatus -> Map.Map OrdinaryRate BracketStart
ordinaryBracketStarts Single =
  Map.fromList
    [ (OrdinaryRate 10, BracketStart 0),
      (OrdinaryRate 12, BracketStart 9950),
      (OrdinaryRate 22, BracketStart 40525),
      (OrdinaryRate 24, BracketStart 86375),
      (OrdinaryRate 32, BracketStart 164925),
      (OrdinaryRate 35, BracketStart 209425),
      (OrdinaryRate 37, BracketStart 523600)
    ]
ordinaryBracketStarts HeadOfHousehold =
  Map.fromList
    [ (OrdinaryRate 10, BracketStart 0),
      (OrdinaryRate 12, BracketStart 14200),
      (OrdinaryRate 22, BracketStart 54200),
      (OrdinaryRate 24, BracketStart 86350),
      (OrdinaryRate 32, BracketStart 164900),
      (OrdinaryRate 35, BracketStart 209400),
      (OrdinaryRate 37, BracketStart 523600)
    ]

bottomRateOnOrdinaryIncome :: FilingStatus -> OrdinaryRate
bottomRateOnOrdinaryIncome fs = head $ Map.keys $ ordinaryBracketStarts fs

topRateOnOrdinaryIncome :: FilingStatus -> OrdinaryRate
topRateOnOrdinaryIncome fs = last $ Map.keys $ ordinaryBracketStarts fs

qualifiedBracketStarts :: FilingStatus -> Map.Map QualifiedRate BracketStart
qualifiedBracketStarts Single =
  Map.fromList
    [ (QualifiedRate 0, BracketStart 0),
      (QualifiedRate 15, BracketStart 40400),
      (QualifiedRate 20, BracketStart 445850)
    ]
qualifiedBracketStarts HeadOfHousehold =
  Map.fromList
    [ (QualifiedRate 0, BracketStart 0),
      (QualifiedRate 15, BracketStart 54100),
      (QualifiedRate 20, BracketStart 473850)
    ]

{- ltcgTaxStart :: FilingStatus -> Int
ltcgTaxStart filingStatus =
  do
    brackets <- Map.lookup filingStatus qualifiedBracketStarts
 -}

distributionPeriods :: Map.Map Age DistributionPeriod
distributionPeriods =
  Map.fromList
    [ (Age 70, 27.4),
      (Age 71, 26.5),
      (Age 72, 25.6),
      (Age 73, 24.7),
      (Age 74, 23.8),
      (Age 75, 22.9),
      (Age 76, 22.0),
      (Age 77, 21.2),
      (Age 78, 20.3),
      (Age 79, 19.5),
      (Age 80, 18.7),
      (Age 81, 17.9),
      (Age 82, 17.1),
      (Age 83, 16.3),
      (Age 84, 15.5),
      (Age 85, 14.8),
      (Age 86, 14.1),
      (Age 87, 13.4),
      (Age 88, 12.7),
      (Age 89, 12.0),
      (Age 90, 11.4),
      (Age 91, 10.8),
      (Age 92, 10.2),
      (Age 93, 9.6),
      (Age 94, 9.1),
      (Age 95, 8.6),
      (Age 96, 8.1),
      (Age 97, 7.6),
      (Age 98, 7.1),
      (Age 99, 6.7),
      (Age 100, 6.3),
      (Age 101, 5.9),
      (Age 102, 5.5),
      (Age 103, 5.2),
      (Age 104, 4.9),
      (Age 105, 4.5),
      (Age 106, 4.2),
      (Age 107, 3.9),
      (Age 108, 3.7),
      (Age 109, 3.4),
      (Age 110, 3.1),
      (Age 111, 2.9),
      (Age 112, 2.6),
      (Age 113, 2.4),
      (Age 114, 2.1)
    ]

rmdFractionForAge :: Age -> Double
rmdFractionForAge age = 1.0 / fromJust (Map.lookup age distributionPeriods)

over65Increment :: Integer
over65Increment = 1350

standardDeduction :: FilingStatus -> StandardDeduction
standardDeduction HeadOfHousehold = StandardDeduction (18800 + over65Increment)
standardDeduction Single = StandardDeduction (12550 + over65Increment)

fail :: () -> a
fail = error "boom"

bracketWidth :: FilingStatus -> OrdinaryRate -> Integer
bracketWidth fs rate =
  fromJust
    ( do
        let brackets = ordinaryBracketStarts fs
        let rates = Map.keys brackets
        let pairs = zip rates (tail rates)
        pair <- List.find (\p -> fst p == rate) pairs
        let successor = snd pair
        rateStart <- Map.lookup rate brackets
        successorStart <- Map.lookup successor brackets
        Just (coerce successorStart - coerce rateStart)
    )

ltcgTaxStart :: FilingStatus -> Integer
ltcgTaxStart fs = coerce (Map.elems (qualifiedBracketStarts fs) !! 1)

taxableSocialSecurityAdjusted :: Year -> FilingStatus -> SocialSecurityBenefits -> SSRelevantIncome -> Double
taxableSocialSecurityAdjusted year filingStatus ssBenefits relevantIncome =
  let unadjusted = taxableSocialSecurity filingStatus ssBenefits relevantIncome
      adjustmentFactor = 1.0 + (0.03 * fromInteger (year - 2021))
      adjusted = unadjusted * adjustmentFactor
   in min adjusted ssBenefits * 0.85

taxableSocialSecurity :: FilingStatus -> SocialSecurityBenefits -> SSRelevantIncome -> Double
taxableSocialSecurity filingStatus ssBenefits relevantIncome =
  let lowBase = case filingStatus of
        Single -> 25000
        HeadOfHousehold -> 25000
      highBase = case filingStatus of
        Single -> 34000
        HeadOfHousehold -> 34000
      combinedIncome = relevantIncome + (ssBenefits / 2.0)
   in f combinedIncome (lowBase, highBase)
  where
    f :: CombinedIncome -> (CombinedIncome, CombinedIncome) -> Double
    f combinedIncome (lowBase, highBase)
      | combinedIncome < lowBase = 0.0
      | combinedIncome < highBase =
        let fractionTaxable = 0.5
            maxSocSecTaxable = ssBenefits * fractionTaxable
         in min ((combinedIncome - lowBase) * fractionTaxable) maxSocSecTaxable
      | otherwise =
        let fractionTaxable = 0.85
            maxSocSecTaxable = ssBenefits * fractionTaxable
         in min (4500 + ((combinedIncome - highBase) * fractionTaxable)) maxSocSecTaxable

applyOrdinaryIncomeBrackets :: FilingStatus -> TaxableOrdinaryIncome -> Double
applyOrdinaryIncomeBrackets fs taxableOrdinaryincome =
  let bracketsDescending = reverse (Map.assocs (ordinaryBracketStarts fs))
   in snd (foldr func (taxableOrdinaryincome, 0.0) bracketsDescending)
  where
    func :: (OrdinaryRate, BracketStart) -> (Double, Double) -> (Double, Double)
    func (rate, BracketStart start) (incomeYetToBeTaxed, taxSoFar) =
      let incomeInThisBracket = nonNeg (incomeYetToBeTaxed - fromInteger start)
          taxInThisBracket = incomeInThisBracket * ordinaryRateAsFraction rate
       in ( nonNeg (incomeYetToBeTaxed - incomeInThisBracket),
            taxSoFar + taxInThisBracket
          )

applyQualifiedBrackets :: FilingStatus -> TaxableOrdinaryIncome -> QualifiedInvestmentIncome -> Double
applyQualifiedBrackets fs taxableOrdinaryIncome qualifiedInvestmentIncome =
  let bracketsDescending = reverse (Map.assocs (qualifiedBracketStarts fs))
   in third (foldr func (taxableOrdinaryIncome, qualifiedInvestmentIncome, 0.0) bracketsDescending)
  where
    totalIncome = taxableOrdinaryIncome + qualifiedInvestmentIncome
    third :: (a, b, c) -> c
    third (_, _, a) = a
    func :: (QualifiedRate, BracketStart) -> (Double, Double, Double) -> (Double, Double, Double)
    func (rate, BracketStart start) (totalIncomeInHigherBrackets, gainsYetToBeTaxed, gainsTaxSoFar) =
      let totalIncomeYetToBeTaxed = nonNeg (totalIncome - totalIncomeInHigherBrackets)
          ordinaryIncomeYetToBeTaxed = nonNeg (totalIncomeYetToBeTaxed - gainsYetToBeTaxed)
          totalIncomeInThisBracket = nonNeg (totalIncomeYetToBeTaxed - fromInteger start)
          ordinaryIncomeInThisBracket = nonNeg (ordinaryIncomeYetToBeTaxed - fromInteger start)
          gainsInThisBracket = nonNeg (totalIncomeInThisBracket - ordinaryIncomeInThisBracket)
          taxInThisBracket = gainsInThisBracket * qualifiedRateAsFraction rate
       in ( totalIncomeInHigherBrackets + totalIncomeInThisBracket,
            nonNeg (gainsYetToBeTaxed - gainsInThisBracket),
            gainsTaxSoFar + taxInThisBracket
          )