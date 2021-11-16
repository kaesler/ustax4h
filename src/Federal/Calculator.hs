{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Federal.Calculator
  ( FederalTaxResults (..),
    makeCalculator,
    taxDue,
    taxDueDebug,
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
    StandardDeduction (..),
    Year,
  )
import Federal.BoundRegime
  ( BoundRegime (..),
    bindRegime,
    netDeduction,
    personalExemptionDeduction,
    standardDeduction,
  )
import Federal.OrdinaryIncome (applyOrdinaryIncomeBrackets)
import Federal.QualifiedIncome (applyQualifiedIncomeBrackets)
import Federal.RMDs ()
import Federal.Regime (Regime)
import qualified Federal.TaxableSocialSecurity as TaxableSocialSecurity
import Math (nonNegSub)

type TaxCalculator = SocSec -> OrdinaryIncome -> QualifiedIncome -> ItemizedDeductions -> FederalTaxResults

makeCalculator :: BoundRegime -> TaxCalculator
makeCalculator br@BoundRegime {..} socSec ordinaryIncome qualifiedIncome itemized =
  let ssRelevantOtherIncome = ordinaryIncome + qualifiedIncome
      taxableSocSec = TaxableSocialSecurity.amountTaxable filingStatus socSec ssRelevantOtherIncome
      StandardDeduction sd = standardDeduction br
      taxableOrdinaryIncome = (taxableSocSec + ordinaryIncome) `nonNegSub` netDeduction br itemized
      taxOnOrdinaryIncome = applyOrdinaryIncomeBrackets ordinaryIncomeBrackets taxableOrdinaryIncome
      taxOnQualifiedIncome = applyQualifiedIncomeBrackets qualifiedIncomeBrackets taxableOrdinaryIncome qualifiedIncome
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
    ssRelevantOtherIncome :: Money,
    taxableSocSec :: Money,
    finalStandardDeduction :: StandardDeduction,
    finalPersonalExemptionDeduction :: Money,
    finalNetDeduction :: Money,
    taxableOrdinaryIncome :: Money,
    taxOnOrdinaryIncome :: Money,
    taxOnQualifiedIncome :: Money
  }
  deriving (Show)

taxResults ::
  Regime ->
  Year ->
  FilingStatus ->
  BirthDate ->
  PersonalExemptions ->
  SocSec ->
  OrdinaryIncome ->
  QualifiedIncome ->
  ItemizedDeductions ->
  FederalTaxResults
taxResults regime year filingStatus birthDate personalExemptions socSec ordinaryIncome qualifiedIncome itemized =
  let boundRegime = bindRegime regime year filingStatus birthDate personalExemptions
      calculator = makeCalculator boundRegime
   in calculator socSec ordinaryIncome qualifiedIncome itemized

taxDue ::
  Regime ->
  Year ->
  FilingStatus ->
  BirthDate ->
  PersonalExemptions ->
  SocSec ->
  OrdinaryIncome ->
  QualifiedIncome ->
  ItemizedDeductions ->
  Double
taxDue regime year filingStatus birthDate personalExemptions socSec ordinaryIncome qualifiedIncome itemized =
  let results = taxResults regime year filingStatus birthDate personalExemptions socSec ordinaryIncome qualifiedIncome itemized
   in taxOnOrdinaryIncome results + taxOnQualifiedIncome results

taxDueDebug ::
  Regime ->
  Year ->
  FilingStatus ->
  BirthDate ->
  PersonalExemptions ->
  SocSec ->
  OrdinaryIncome ->
  QualifiedIncome ->
  ItemizedDeductions ->
  IO ()
taxDueDebug regime year filingStatus birthDate personalExemptions socSec ordinaryIncome qualifiedIncome itemized =
  let r = taxResults regime year filingStatus birthDate personalExemptions socSec ordinaryIncome qualifiedIncome itemized
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
        putStrLn ("  result: " ++ show (taxOnOrdinaryIncome r + taxOnQualifiedIncome r))
