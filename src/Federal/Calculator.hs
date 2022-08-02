{-# LANGUAGE RecordWildCards #-}

module Federal.Calculator
  ( FederalTaxResults (..),
    makeCalculator,
    taxDueForFutureYear,
    taxDueForKnownYear,
    taxDueForKnownYearDebug,
    taxResultsForFutureYear,
    taxResultsForKnownYear,
  )
where

import CommonTypes
  ( BirthDate,
    FilingStatus,
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

makeCalculator :: BoundRegime -> BirthDate -> PersonalExemptions -> TaxCalculator
makeCalculator br@BoundRegime {..} birthDate pe socSec ordinaryIncome qualifiedIncome itemized =
  let ssRelevantOtherIncome = ordinaryIncome <> qualifiedIncome
      taxableSocSec = TaxableSocialSecurity.amountTaxable filingStatus socSec ssRelevantOtherIncome
      taxableOrdinaryIncome = (taxableSocSec <> ordinaryIncome) `applyDeductions` netDeduction br birthDate pe itemized
      taxOnOrdinaryIncome = TFS.taxDueOnOrdinaryIncome ordinaryBrackets taxableOrdinaryIncome
      taxOnQualifiedIncome =
        TFS.taxDueOnQualifiedIncome qualifiedBrackets taxableOrdinaryIncome (asTaxable qualifiedIncome)
   in FederalTaxResults
        { boundRegime = br,
          ssRelevantOtherIncome = ssRelevantOtherIncome,
          taxableSocSec = taxableSocSec,
          finalStandardDeduction = standardDeduction br birthDate,
          finalPersonalExemptionDeduction = personalExemptionDeduction br pe,
          finalNetDeduction = netDeduction br birthDate pe itemized,
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
  FilingStatus -> 
  BirthDate ->
  PersonalExemptions ->
  SocSec ->
  OrdinaryIncome ->
  QualifiedIncome ->
  ItemizedDeductions ->
  FederalTaxResults
taxResultsForKnownYear year filingStatus birthDate personalExemptions socSec ordinaryIncome qualifiedIncome itemized =
  let boundRegime = boundRegimeForKnownYear year filingStatus
      calculator = makeCalculator boundRegime birthDate personalExemptions
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
  let results = taxResultsForKnownYear year filingStatus birthDate personalExemptions socSec ordinaryIncome qualifiedIncome itemized
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
  let r = taxResultsForKnownYear year filingStatus birthDate personalExemptions socSec ordinaryIncome qualifiedIncome itemized
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

taxResultsForFutureYear ::
  Regime ->
  Year ->
  Double ->
  FilingStatus ->
  BirthDate ->
  PersonalExemptions ->
  SocSec ->
  OrdinaryIncome ->
  QualifiedIncome ->
  ItemizedDeductions ->
  FederalTaxResults
taxResultsForFutureYear reg futureYear estimate filingStatus birthDate personalExemptions socSec ordinaryIncome qualifiedIncome itemized =
  let boundRegime = boundRegimeForFutureYear reg futureYear estimate filingStatus
      calculator = makeCalculator boundRegime birthDate personalExemptions
   in calculator socSec ordinaryIncome qualifiedIncome itemized

taxDueForFutureYear ::
  Regime ->
  Year ->
  Double ->
  FilingStatus ->
  BirthDate ->
  PersonalExemptions ->
  SocSec ->
  OrdinaryIncome ->
  QualifiedIncome ->
  ItemizedDeductions ->
  TaxPayable
taxDueForFutureYear regime futureYear inflationEstimate filingStatus birthDate personalExemptions socSec ordinaryIncome qualifiedIncome itemized =
  let results = taxResultsForFutureYear regime futureYear inflationEstimate filingStatus birthDate personalExemptions socSec ordinaryIncome qualifiedIncome itemized
   in taxOnOrdinaryIncome results <> taxOnQualifiedIncome results
