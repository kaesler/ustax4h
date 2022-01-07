module Federal.TaxFunctions
  ( taxDueOnOrdinaryIncome,
    taxDueOnQualifiedIncome,
  )
where

import qualified Federal.OrdinaryBrackets as OB
import Federal.QualifiedBrackets as QB
  ( QualifiedBrackets,
    taxFunctionFor,
  )
import qualified Federal.QualifiedBrackets as QB
import Moneys (TaxPayable, TaxableIncome, asTaxable, reduceBy)
import TaxFunction (bracketsTaxFunction)

taxDueOnOrdinaryIncome :: OB.OrdinaryBrackets -> TaxableIncome -> TaxPayable
taxDueOnOrdinaryIncome = OB.taxFunctionFor

taxDueOnQualifiedIncome :: QualifiedBrackets -> TaxableIncome -> TaxableIncome -> TaxPayable
taxDueOnQualifiedIncome brackets taxableOrdinaryIncome qualifiedIncome =
  let taxFunction = QB.taxFunctionFor brackets
      taxOnBoth = taxFunction $ taxableOrdinaryIncome <> qualifiedIncome
      taxOnOrdinary = taxFunction taxableOrdinaryIncome
   in taxOnBoth `reduceBy` taxOnOrdinary