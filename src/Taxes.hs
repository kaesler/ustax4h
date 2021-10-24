module Taxes
  ( Age (..),
    FilingStatus (..),
    OrdinaryIncome,
    OrdinaryRate (..),
    QualifiedIncome,
    QualifiedRate (..),
    SSRelevantOtherIncome,
    SocSec,
    StandardDeduction (..),
    Year,
    applyOrdinaryIncomeBrackets,
    applyQualifiedIncomeBrackets,
    bottomRateOnOrdinaryIncome,
    federalTaxDue,
    federalTaxDueDebug,
    incomeToEndOfOrdinaryBracket,
    maStateTaxDue,
    ordinaryIncomeBracketWidth,
    ordinaryRateAsFraction,
    ordinaryRatesExceptTop,
    rmdFractionForAge,
    roundHalfUp,
    standardDeduction,
    startOfNonZeroQualifiedRateBracket,
    taxToEndOfOrdinaryBracket,
    taxableSocialSecurity,
    taxableSocialSecurityAdjusted,
    topRateOnOrdinaryIncome,
  )
where

import CommonTypes
  ( Age (..),
    DistributionPeriod,
    FilingStatus (..),
    OrdinaryIncome,
    QualifiedIncome,
    SSRelevantOtherIncome,
    SocSec,
    Year,
  )
import Data.Coerce (coerce)
import qualified Data.List as List
import Data.List.NonEmpty as NonEmpty (fromList, head, last, reverse, tail, takeWhile, toList, (!!))
import Data.Map.NonEmpty as NEMap (NEMap, assocs, elems, fromList, keys, lookup)
import qualified Data.Map.Strict ()
import Data.Maybe (fromJust)
import MAStateTax (maStateTaxDue)
import Math (nonNegSub, roundHalfUp)
import RMDs (rmdFractionForAge)
import TaxableSocialSecurity (taxableSocialSecurity)

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

data FederalTaxResults = FederalTaxResults
  { ssRelevantOtherIncome :: Double,
    taxableSocSec :: Double,
    stdDeduction :: StandardDeduction,
    taxableOrdinaryIncome :: Double,
    taxOnOrdinaryIncome :: Double,
    taxOnQualifiedIncome :: Double
  }
  deriving (Show)

federalTaxResults :: Year -> FilingStatus -> SocSec -> OrdinaryIncome -> QualifiedIncome -> FederalTaxResults
federalTaxResults year filingStatus socSec ordinaryIncome qualifiedIncome =
  let ssRelevantOtherIncome = ordinaryIncome + qualifiedIncome
      taxableSocSec = taxableSocialSecurity filingStatus socSec ssRelevantOtherIncome
      StandardDeduction sd = standardDeduction year filingStatus
      taxableOrdinaryIncome = (taxableSocSec + ordinaryIncome) `nonNegSub` fromInteger sd
      taxOnOrdinaryIncome = applyOrdinaryIncomeBrackets year filingStatus taxableOrdinaryIncome
      taxOnQualifiedIncome = applyQualifiedIncomeBrackets filingStatus taxableOrdinaryIncome qualifiedIncome
   in FederalTaxResults
        { ssRelevantOtherIncome = ssRelevantOtherIncome,
          taxableSocSec = taxableSocSec,
          stdDeduction = standardDeduction year filingStatus,
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
  let r = federalTaxResults year filingStatus socSec ordinaryIncome qualifiedIncome
   in do
        putStrLn "Inputs"
        putStrLn (" fs: " ++ show filingStatus)
        putStrLn (" socSec: " ++ show socSec)
        putStrLn (" ordinaryIncome: " ++ show ordinaryIncome)
        putStrLn (" qualifiedIncome: " ++ show qualifiedIncome)
        putStrLn "Outputs"
        putStrLn ("  ssRelevantOtherIncome: " ++ show (ssRelevantOtherIncome r))
        putStrLn ("  taxableSocSec: " ++ show (taxableSocSec r))
        putStrLn ("  standardDeduction: " ++ show (stdDeduction r))
        putStrLn ("  taxableOrdinaryIncome: " ++ show (taxableOrdinaryIncome r))
        putStrLn ("  taxOnOrdinaryIncome: " ++ show (taxOnOrdinaryIncome r))
        putStrLn ("  taxOnQualifiedIncome: " ++ show (taxOnQualifiedIncome r))
        putStrLn ("  result: " ++ show (taxOnOrdinaryIncome r + taxOnQualifiedIncome r))

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

-----------------------------------------
bottomRateOnOrdinaryIncome :: Year -> FilingStatus -> OrdinaryRate
bottomRateOnOrdinaryIncome year fs = NonEmpty.head $ NEMap.keys $ ordinaryBracketStarts year fs

topRateOnOrdinaryIncome :: Year -> FilingStatus -> OrdinaryRate
topRateOnOrdinaryIncome year fs = NonEmpty.last $ NEMap.keys $ ordinaryBracketStarts year fs

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

over65Increment :: Integer
over65Increment = 1350

standardDeduction :: Year -> FilingStatus -> StandardDeduction
standardDeduction _ HeadOfHousehold = StandardDeduction (18800 + over65Increment)
standardDeduction _ Single = StandardDeduction (12550 + over65Increment)

-- fail :: () -> a
-- fail = error "boom"

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
