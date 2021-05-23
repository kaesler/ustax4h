module Taxes
  ( Age (..),
    FilingStatus (..),
    OrdinaryRate (..),
    QualifiedRate (..),
    QualifiedIncome,
    SocSec,
    SSRelevantOtherIncome,
    StandardDeduction (..),
    OrdinaryIncome,
    applyOrdinaryIncomeBrackets,
    applyQualifiedBrackets,
    bottomRateOnOrdinaryIncome,
    bracketWidth,
    federalTaxDue,
    federalTaxDueDebug,
    incomeToEndOfOrdinaryBracket,
    startOfNonZeroQualifiedRateBracket,
    ordinaryRateAsFraction,
    ordinaryRatesExceptTop,
    rmdFractionForAge,
    roundHalfUp,
    standardDeduction,
    taxableSocialSecurity,
    taxableSocialSecurityAdjusted,
    taxToEndOfOrdinaryBracket,
    topRateOnOrdinaryIncome,
  )
where

import Data.Coerce (coerce)
import qualified Data.List as List
import Data.List.NonEmpty as NonEmpty (fromList, head, last, reverse, tail, takeWhile, toList, (!!))
import Data.Map.NonEmpty as NEMap (NEMap, assocs, elems, fromList, keys, lookup)
import qualified Data.Map.Strict ()
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

type SocSec = Double

type SSRelevantOtherIncome = Double

type CombinedIncome = Double

type OrdinaryIncome = Double

type QualifiedIncome = Double

type DistributionPeriod = Double

type MassachusettsGrossIncome = Double

data FederalTaxResults = FederalTaxResults
  { ssRelevantOtherIncome :: Double,
    taxableSocSec :: Double,
    stdDeduction :: StandardDeduction,
    taxableOrdinaryIncome :: Double,
    taxOnOrdinaryIncome :: Double,
    taxOnQualifiedIncome :: Double
  }
  deriving (Show)

nonNeg :: Double -> Double
nonNeg x
  | x < 0.0 = 0.0
  | otherwise = x

roundHalfUp :: Double -> Double
roundHalfUp x =
  let xAbs = abs x
      sign = if x >= 0.0 then 1.0 else (-1.0)
      (whole, frac) = properFraction xAbs
   in (sign *) $ fromInteger $ if frac >= 0.5 then whole + 1 else whole

maStateTaxRate :: Double
maStateTaxRate = 0.05

maStateTaxDue :: Year -> FilingStatus -> MassachusettsGrossIncome -> Double
maStateTaxDue year filingStatus maGrossIncome =
  let personalExemption = if filingStatus == HeadOfHousehold then 6800 else 4400
      ageExemption = 700
      dependents = if filingStatus == HeadOfHousehold then 1 else 0
      dependentsExemption = 1000.0 * dependents
   in maStateTaxRate * nonNeg (maGrossIncome - personalExemption - ageExemption - dependentsExemption)

federalTaxResults :: Year -> FilingStatus -> SocSec -> OrdinaryIncome -> QualifiedIncome -> FederalTaxResults
federalTaxResults year filingStatus socSec ordinaryIncome qualifiedIncome =
  let ssRelevantOtherIncome = ordinaryIncome + qualifiedIncome
      taxableSocSec = taxableSocialSecurity filingStatus socSec ssRelevantOtherIncome
      StandardDeduction sd = standardDeduction filingStatus
      taxableOrdinaryIncome = nonNeg (taxableSocSec + ordinaryIncome - fromInteger sd)
      taxOnOrdinaryIncome = applyOrdinaryIncomeBrackets filingStatus taxableOrdinaryIncome
      taxOnQualifiedIncome = applyQualifiedBrackets filingStatus taxableOrdinaryIncome qualifiedIncome
   in FederalTaxResults
        { ssRelevantOtherIncome = ssRelevantOtherIncome,
          taxableSocSec = taxableSocSec,
          stdDeduction = standardDeduction filingStatus,
          taxableOrdinaryIncome = taxableOrdinaryIncome,
          taxOnOrdinaryIncome = taxOnOrdinaryIncome,
          taxOnQualifiedIncome = taxOnQualifiedIncome
        }

federalTaxDue :: Year -> FilingStatus -> SocSec -> OrdinaryIncome -> QualifiedIncome -> Double
federalTaxDue year filingStatus socSec ordinaryIncome qualifiedIncome =
  let results = federalTaxResults year filingStatus socSec ordinaryIncome qualifiedIncome
   in taxOnOrdinaryIncome results + taxOnQualifiedIncome results

federalTaxDueDebug :: Year -> FilingStatus -> SocSec -> OrdinaryIncome -> QualifiedIncome -> IO ()
federalTaxDueDebug year filingStatus socSec ordinaryIncome qualifiedIncome =
  let ssRelevantOtherIncome = ordinaryIncome + qualifiedIncome
      taxableSocSec = taxableSocialSecurity filingStatus socSec ssRelevantOtherIncome
      StandardDeduction sd = standardDeduction filingStatus
      taxableOrdinaryIncome = nonNeg (taxableSocSec + ordinaryIncome - fromInteger sd)
      taxOnOrdinaryIncome = applyOrdinaryIncomeBrackets filingStatus taxableOrdinaryIncome
      taxOnQualifiedIncome = applyQualifiedBrackets filingStatus taxableOrdinaryIncome qualifiedIncome
      result = taxOnOrdinaryIncome + taxOnQualifiedIncome
   in do
        putStrLn "Inputs"
        putStrLn (" fs: " ++ show filingStatus)
        putStrLn (" socSec: " ++ show socSec)
        putStrLn (" ordinaryIncome: " ++ show ordinaryIncome)
        putStrLn (" qualifiedIncome: " ++ show qualifiedIncome)
        putStrLn "Outputs"
        putStrLn ("  ssRelevantOtherIncome: " ++ show ssRelevantOtherIncome)
        putStrLn ("  taxableSocSec: " ++ show taxableSocSec)
        putStrLn ("  standardDeduction: " ++ show sd)
        putStrLn ("  taxableOrdinaryIncome: " ++ show taxableOrdinaryIncome)
        putStrLn ("  taxOnOrdinaryIncome: " ++ show taxOnOrdinaryIncome)
        putStrLn ("  taxOnQualifiedIncome: " ++ show taxOnQualifiedIncome)
        putStrLn ("  result: " ++ show result)

ordinaryBracketStarts :: FilingStatus -> NEMap OrdinaryRate BracketStart
ordinaryBracketStarts Single =
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
ordinaryBracketStarts HeadOfHousehold =
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

ordinaryRateSuccessor :: FilingStatus -> OrdinaryRate -> Maybe OrdinaryRate
ordinaryRateSuccessor fs rate =
  do
    let brackets = ordinaryBracketStarts fs
    let rates = NEMap.keys brackets
    let pairs = Prelude.zip (toList rates) (NonEmpty.tail rates)
    pair <- List.find (\p -> fst p == rate) pairs
    Just (snd pair)

-------- For tests ---------------------
ordinaryRatesExceptTop :: FilingStatus -> [OrdinaryRate]
ordinaryRatesExceptTop fs =
  let brackets = ordinaryBracketStarts fs
      rates = NEMap.keys brackets
      topRate = NonEmpty.last rates
   in NonEmpty.takeWhile (/= topRate) rates

incomeToEndOfOrdinaryBracket :: FilingStatus -> OrdinaryRate -> Double
incomeToEndOfOrdinaryBracket filingStatus bracketRate =
  let bracketStarts = ordinaryBracketStarts filingStatus
      successorRate = fromJust (ordinaryRateSuccessor filingStatus bracketRate)
      BracketStart startOfSuccessor = fromJust (NEMap.lookup successorRate bracketStarts)
      StandardDeduction deduction = standardDeduction filingStatus
   in fromInteger (startOfSuccessor + deduction)

taxToEndOfOrdinaryBracket :: FilingStatus -> OrdinaryRate -> Double
taxToEndOfOrdinaryBracket filingStatus bracketRate =
  let relevantRates = List.takeWhile (<= bracketRate) (ordinaryRatesExceptTop filingStatus)
      bracketWidths = List.map (bracketWidth filingStatus) relevantRates
      pairs = relevantRates `zip` bracketWidths
      taxesDue = List.map taxForBracket pairs
        where
          taxForBracket (OrdinaryRate r, width) =
            fromIntegral width * fromInteger r / 100.0
   in List.sum taxesDue

-----------------------------------------
bottomRateOnOrdinaryIncome :: FilingStatus -> OrdinaryRate
bottomRateOnOrdinaryIncome fs = NonEmpty.head $ NEMap.keys $ ordinaryBracketStarts fs

topRateOnOrdinaryIncome :: FilingStatus -> OrdinaryRate
topRateOnOrdinaryIncome fs = NonEmpty.last $ NEMap.keys $ ordinaryBracketStarts fs

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

distributionPeriods :: NEMap Age DistributionPeriod
distributionPeriods =
  NEMap.fromList $
    NonEmpty.fromList
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
rmdFractionForAge age = 1.0 / fromJust (NEMap.lookup age distributionPeriods)

over65Increment :: Integer
over65Increment = 1350

standardDeduction :: FilingStatus -> StandardDeduction
standardDeduction HeadOfHousehold = StandardDeduction (18800 + over65Increment)
standardDeduction Single = StandardDeduction (12550 + over65Increment)

-- fail :: () -> a
-- fail = error "boom"

bracketWidth :: FilingStatus -> OrdinaryRate -> Integer
bracketWidth fs rate =
  fromJust
    ( do
        let brackets = ordinaryBracketStarts fs
        let rates = NEMap.keys brackets
        let pairs = Prelude.zip (toList rates) (NonEmpty.tail rates)
        pair <- List.find (\p -> fst p == rate) pairs
        let successor = snd pair
        rateStart <- NEMap.lookup rate brackets
        successorStart <- NEMap.lookup successor brackets
        Just (coerce successorStart - coerce rateStart)
    )

startOfNonZeroQualifiedRateBracket :: FilingStatus -> Integer
startOfNonZeroQualifiedRateBracket fs =
  -- The start of the 2nd-to-bottom bracket.
  coerce $ (NonEmpty.!!) (NEMap.elems (qualifiedBracketStarts fs)) 1

taxableSocialSecurityAdjusted :: Year -> FilingStatus -> SocSec -> SSRelevantOtherIncome -> Double
taxableSocialSecurityAdjusted year filingStatus ssBenefits relevantIncome =
  let unadjusted = taxableSocialSecurity filingStatus ssBenefits relevantIncome
      adjustmentFactor = 1.0 + (0.03 * fromInteger (year - 2021))
      adjusted = unadjusted * adjustmentFactor
   in min adjusted ssBenefits * 0.85

taxableSocialSecurity :: FilingStatus -> SocSec -> SSRelevantOtherIncome -> Double
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

applyOrdinaryIncomeBrackets :: FilingStatus -> OrdinaryIncome -> Double
applyOrdinaryIncomeBrackets fs taxableOrdinaryincome =
  let bracketsDescending = NonEmpty.reverse (NEMap.assocs (ordinaryBracketStarts fs))
   in snd (List.foldl func (taxableOrdinaryincome, 0.0) bracketsDescending)
  where
    func :: (Double, Double) -> (OrdinaryRate, BracketStart) -> (Double, Double)
    func (incomeYetToBeTaxed, taxSoFar) (rate, BracketStart start) =
      let incomeInThisBracket = nonNeg (incomeYetToBeTaxed - fromInteger start)
          taxInThisBracket = incomeInThisBracket * ordinaryRateAsFraction rate
       in ( nonNeg (incomeYetToBeTaxed - incomeInThisBracket),
            taxSoFar + taxInThisBracket
          )

applyQualifiedBrackets :: FilingStatus -> OrdinaryIncome -> QualifiedIncome -> Double
applyQualifiedBrackets fs taxableOrdinaryIncome qualifiedInvestmentIncome =
  let bracketsDescending = NonEmpty.reverse (NEMap.assocs (qualifiedBracketStarts fs))
   in third (List.foldl func (0.0, qualifiedInvestmentIncome, 0.0) bracketsDescending)
  where
    totalTaxableIncome = taxableOrdinaryIncome + qualifiedInvestmentIncome
    third :: (a, b, c) -> c
    third (_, _, a) = a
    func :: (Double, Double, Double) -> (QualifiedRate, BracketStart) -> (Double, Double, Double)
    func (totalIncomeInHigherBrackets, gainsYetToBeTaxed, gainsTaxSoFar) (rate, BracketStart start) =
      let totalIncomeYetToBeTaxed = nonNeg (totalTaxableIncome - totalIncomeInHigherBrackets)
          ordinaryIncomeYetToBeTaxed = nonNeg (totalIncomeYetToBeTaxed - gainsYetToBeTaxed)
          totalIncomeInThisBracket = nonNeg (totalIncomeYetToBeTaxed - fromInteger start)
          ordinaryIncomeInThisBracket = nonNeg (ordinaryIncomeYetToBeTaxed - fromInteger start)
          gainsInThisBracket = nonNeg (totalIncomeInThisBracket - ordinaryIncomeInThisBracket)
          taxInThisBracket = gainsInThisBracket * qualifiedRateAsFraction rate
       in ( totalIncomeInHigherBrackets + totalIncomeInThisBracket,
            nonNeg (gainsYetToBeTaxed - gainsInThisBracket),
            gainsTaxSoFar + taxInThisBracket
          )
