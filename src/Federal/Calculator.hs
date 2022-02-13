{-# LANGUAGE RecordWildCards #-}

module Federal.Calculator
  ( FederalTaxResults (..),
    makeCalculator,
    taxDueForKnownYear,
    taxResultsForFutureYear,
    taxResultsForKnownYear,
    taxDueForKnownYearDebug,
  )
where

import CommonTypes
  ( BirthDate,
    FilingStatus,
    InflationEstimate,
    Year,
  )
import Federal.BoundRegime
  ( BoundRegime (..),
    boundRegimeForFutureYear,
    boundRegimeForKnownYear,
    netDeduction,
    personalExemptionDeduction,
    standardDeduction,
  )
import Federal.RMDs ()
import Federal.Regime (Regime)
import qualified Federal.TaxFunctions as TFS
import qualified Federal.TaxableSocialSecurity as TaxableSocialSecurity
import Federal.Types
  ( ItemizedDeductions,
    OrdinaryIncome,
    PersonalExemptions,
    QualifiedIncome,
    SocSec,
  )
import Moneys
  ( Deduction,
    Income,
    TaxPayable,
    TaxableIncome,
    applyDeductions,
    asTaxable,
  )

type TaxCalculator = SocSec -> OrdinaryIncome -> QualifiedIncome -> ItemizedDeductions -> FederalTaxResults

makeCalculator :: BoundRegime -> TaxCalculator
makeCalculator br@BoundRegime {..} socSec ordinaryIncome qualifiedIncome itemized =
  let ssRelevantOtherIncome = ordinaryIncome <> qualifiedIncome
      taxableSocSec = TaxableSocialSecurity.amountTaxable filingStatus socSec ssRelevantOtherIncome
      taxableOrdinaryIncome = (taxableSocSec <> ordinaryIncome) `applyDeductions` netDeduction br itemized
      taxOnOrdinaryIncome = TFS.taxDueOnOrdinaryIncome ordinaryBrackets taxableOrdinaryIncome
      taxOnQualifiedIncome =
        TFS.taxDueOnQualifiedIncome qualifiedBrackets taxableOrdinaryIncome (asTaxable qualifiedIncome)
   in FederalTaxResults
        { boundRegime = br,
          ssRelevantOtherIncome = ssRelevantOtherIncome,
          taxableSocSec = taxableSocSec,
          finalStandardDeduction = standardDeduction br,
          finalPersonalExemptionDeduction = personalExemptionDeduction br,
          finalNetDeduction = netDeduction br itemized,
          taxableOrdinaryIncome = taxableOrdinaryIncome,
          taxOnOrdinaryIncome = taxOnOrdinaryIncome,
          taxOnQualifiedIncome = taxOnQualifiedIncome
        }

data FederalTaxResults = FederalTaxResults
  { boundRegime :: BoundRegime,
    ssRelevantOtherIncome :: Income,
    taxableSocSec :: Income,
    finalStandardDeduction :: Deduction,
    finalPersonalExemptionDeduction :: Deduction,
    finalNetDeduction :: Deduction,
    taxableOrdinaryIncome :: TaxableIncome,
    taxOnOrdinaryIncome :: TaxPayable,
    taxOnQualifiedIncome :: TaxPayable
  }
  deriving (Show)

taxResultsForKnownYear ::
  Year ->
  BirthDate ->
  FilingStatus ->
  PersonalExemptions ->
  SocSec ->
  OrdinaryIncome ->
  QualifiedIncome ->
  ItemizedDeductions ->
  FederalTaxResults
taxResultsForKnownYear year birthDate filingStatus personalExemptions socSec ordinaryIncome qualifiedIncome itemized =
  let boundRegime = boundRegimeForKnownYear year birthDate filingStatus personalExemptions
      calculator = makeCalculator boundRegime
   in calculator socSec ordinaryIncome qualifiedIncome itemized

taxResultsForFutureYear ::
  Regime ->
  InflationEstimate ->
  BirthDate ->
  FilingStatus ->
  PersonalExemptions ->
  SocSec ->
  OrdinaryIncome ->
  QualifiedIncome ->
  ItemizedDeductions ->
  FederalTaxResults
taxResultsForFutureYear reg estimate birthDate filingStatus personalExemptions socSec ordinaryIncome qualifiedIncome itemized =
  let boundRegime = boundRegimeForFutureYear reg estimate birthDate filingStatus personalExemptions
      calculator = makeCalculator boundRegime
   in calculator socSec ordinaryIncome qualifiedIncome itemized

taxDueForKnownYear ::
  Year ->
  FilingStatus ->
  BirthDate ->
  PersonalExemptions ->
  SocSec ->
  OrdinaryIncome ->
  QualifiedIncome ->
  ItemizedDeductions ->
  TaxPayable
taxDueForKnownYear year filingStatus birthDate personalExemptions socSec ordinaryIncome qualifiedIncome itemized =
  let results = taxResultsForKnownYear year birthDate filingStatus personalExemptions socSec ordinaryIncome qualifiedIncome itemized
   in taxOnOrdinaryIncome results <> taxOnQualifiedIncome results

taxDueForKnownYearDebug ::
  Year ->
  FilingStatus ->
  BirthDate ->
  PersonalExemptions ->
  SocSec ->
  OrdinaryIncome ->
  QualifiedIncome ->
  ItemizedDeductions ->
  IO ()
taxDueForKnownYearDebug year filingStatus birthDate personalExemptions socSec ordinaryIncome qualifiedIncome itemized =
  let r = taxResultsForKnownYear year birthDate filingStatus personalExemptions socSec ordinaryIncome qualifiedIncome itemized
   in do
        putStrLn "Inputs"
        putStrLn (" fs: " ++ show filingStatus)
        putStrLn (" socSec: " ++ show socSec)
        putStrLn (" ordinaryIncome: " ++ show ordinaryIncome)
        putStrLn (" qualifiedIncome: " ++ show qualifiedIncome)
        putStrLn (" itemizedDeductions: " ++ show itemized)
        putStrLn "Outputs"
        putStrLn ("  ssRelevantOtherIncome: " ++ show (ssRelevantOtherIncome r))
        putStrLn ("  taxableSocSec: " ++ show (taxableSocSec r))
        putStrLn ("  finalStandardDeduction: " ++ show (finalStandardDeduction r))
        putStrLn ("  finalNetDeduction: " ++ show (finalNetDeduction r))
        putStrLn ("  taxableOrdinaryIncome: " ++ show (taxableOrdinaryIncome r))
        putStrLn ("  taxOnOrdinaryIncome: " ++ show (taxOnOrdinaryIncome r))
        putStrLn ("  taxOnQualifiedIncome: " ++ show (taxOnQualifiedIncome r))
        putStrLn ("  result: " ++ show (taxOnOrdinaryIncome r <> taxOnQualifiedIncome r))
