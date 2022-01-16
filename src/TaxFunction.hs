{-# LANGUAGE DerivingStrategies #-}

module TaxFunction
  ( TaxFunction,
    bracketsTaxFunction,
    flatTaxFunction,
  )
where

import Brackets (Brackets)
import Data.List.NonEmpty (NonEmpty, zip, zipWith, (<|))
import Data.Map.NonEmpty (elems, keys, toList)
import Moneys (IncomeThreshold, TaxPayable, TaxableIncome, amountOverThreshold, applyTaxRate)
import TaxRate (TaxRate (absoluteDifference, zeroRate))

type TaxFunction = TaxableIncome -> TaxPayable

thresholdTaxFunction :: TaxRate r => IncomeThreshold -> r -> TaxFunction
thresholdTaxFunction threshold rate ti = applyTaxRate rate (ti `amountOverThreshold` threshold)

flatTaxFunction :: TaxRate r => r -> TaxFunction
flatTaxFunction = thresholdTaxFunction mempty

bracketsTaxFunction :: TaxRate r => Brackets r -> TaxFunction
bracketsTaxFunction brackets =
  let pairs = rateDeltasForBrackets brackets
      taxFuncs = fmap (uncurry thresholdTaxFunction) pairs
   in foldl1 mappend taxFuncs

rateDeltasForBrackets :: TaxRate r => Brackets r -> NonEmpty (IncomeThreshold, r)
rateDeltasForBrackets brackets =
  let pairs = toList brackets
      rates = fmap fst pairs
      thresholds = fmap snd pairs
      deltas = Data.List.NonEmpty.zipWith absoluteDifference (zeroRate <| rates) rates
   in Data.List.NonEmpty.zip thresholds deltas
