{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Federal.Calculator
  ( federalTaxDue,
    federalTaxDueDebug,
  )
where

import CommonTypes
  ( BirthDate,
    FilingStatus,
    ItemizedDeductions,
    Money,
    OrdinaryIncome,
    PersonalExemptions,
    QualifiedIncome,
    SocSec,
    Year,
  )
import Federal.Deductions
  ( StandardDeduction (..),
    standardDeductionFor,
  )
import Federal.OrdinaryIncome ( applyOrdinaryIncomeBrackets, ordinaryIncomeBracketsFor )
import Federal.QualifiedIncome (applyQualifiedIncomeBrackets)
import Federal.Regime(RegimeKind(..), BoundRegime(..))
import Federal.RMDs ()
import Federal.TaxableSocialSecurity (taxableSocialSecurity)
import Math (nonNegSub)
import StateMA.Calculator (maStateTaxDue)

type TaxCalculator = SocSec -> OrdinaryIncome -> QualifiedIncome -> ItemizedDeductions -> FederalTaxResults

makeCalculator :: BoundRegime -> TaxCalculator
makeCalculator BoundRegime{..} = undefined

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
      StandardDeduction sd = standardDeductionFor year filingStatus
      taxableOrdinaryIncome = (taxableSocSec + ordinaryIncome) `nonNegSub` fromInteger sd
      brackets = ordinaryIncomeBracketsFor year filingStatus
      taxOnOrdinaryIncome = applyOrdinaryIncomeBrackets brackets taxableOrdinaryIncome
      taxOnQualifiedIncome = applyQualifiedIncomeBrackets filingStatus taxableOrdinaryIncome qualifiedIncome
   in FederalTaxResults
        { ssRelevantOtherIncome = ssRelevantOtherIncome,
          taxableSocSec = taxableSocSec,
          stdDeduction = standardDeductionFor year filingStatus,
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