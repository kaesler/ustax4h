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
import Federal.BracketTypes
import Federal.Deductions
  ( StandardDeduction (..),
    standardDeduction,
  )
import Federal.OrdinaryIncome
import Federal.RMDs (rmdFractionForAge)
import Federal.TaxableSocialSecurity (taxableSocialSecurity, taxableSocialSecurityAdjusted)
import Math (nonNegSub, roundHalfUp)
import State.MAStateTax (maStateTaxDue)

newtype QualifiedRate = QualifiedRate Integer
  deriving (Eq, Ord, Show)

qualifiedRateAsFraction :: QualifiedRate -> Double
qualifiedRateAsFraction (QualifiedRate r) = fromIntegral r / 100.0

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
