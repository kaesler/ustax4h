{-# LANGUAGE DerivingStrategies #-}

module TaxFunction
  ( bracketsTaxFunction,
    flatTaxFunction,
  )
where

import Brackets (Brackets)
import Data.List.NonEmpty (NonEmpty, zip, zipWith, (<|))
import Data.Map.NonEmpty (elems, keys)
import Money.Money (IncomeThreshold, TaxPayable, TaxableIncome, amountAbove, applyTaxRate)
import TaxRate (TaxRate (absoluteDifference, zero))

type TaxFunction = TaxableIncome -> TaxPayable

thresholdTaxFunction :: TaxRate r => IncomeThreshold -> r -> TaxFunction
thresholdTaxFunction threshold rate ti = applyTaxRate rate (amountAbove ti threshold)

flatTaxFunction :: TaxRate r => r -> TaxFunction
flatTaxFunction = thresholdTaxFunction mempty

bracketsTaxFunction :: TaxRate r => Brackets r -> TaxFunction
bracketsTaxFunction brackets =
  let pairs = rateDeltasForBrackets brackets
      taxFuncs = fmap (uncurry thresholdTaxFunction) pairs
   in foldl1 mappend taxFuncs

rateDeltasForBrackets :: TaxRate r => Brackets r -> NonEmpty (IncomeThreshold, r)
rateDeltasForBrackets brackets =
  let rates = keys brackets
      deltas = Data.List.NonEmpty.zipWith absoluteDifference (zero <| rates) rates
      thresholds = elems brackets
   in Data.List.NonEmpty.zip thresholds deltas
