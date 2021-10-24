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
import Federal.BracketTypes ()
import Federal.Deductions
  ( StandardDeduction (..),
    standardDeduction,
  )
import Federal.OrdinaryIncome
import Federal.QualifiedIncome
import Federal.RMDs (rmdFractionForAge)
import Federal.TaxableSocialSecurity (taxableSocialSecurity, taxableSocialSecurityAdjusted)
import Math (nonNegSub, roundHalfUp)
import StateMA.Calculator (maStateTaxDue)

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
