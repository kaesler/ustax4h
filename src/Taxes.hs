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
import Federal.Calculator
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
