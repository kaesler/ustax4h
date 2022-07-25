module FutureYearTestDataFromScala
  ( TestCase (..),
    cases,
  )
where

import CommonTypes
  ( BirthDate,
    FilingStatus (..),
    Year,
  )
import Data.Time (fromGregorian)
import Federal.Regime (Regime (..))
import Federal.Types (QualifiedIncome, SSRelevantOtherIncome, SocSec)
import Moneys (Deduction, TaxPayable, makeFromInt)

data TestCase = TestCase
  { regime :: Regime,
    year :: Year,
    estimatedAnnualInflationFactor :: Double,
    birthDate :: BirthDate,
    dependents :: Int,
    filingStatus :: FilingStatus,
    socSec :: SocSec,
    ordinaryIncomeNonSS :: SSRelevantOtherIncome,
    qualifiedIncome :: QualifiedIncome,
    itemizedDeductions :: Deduction,
    expectedFederalTax :: TaxPayable,
    expectedStateTax :: TaxPayable
  }
  deriving (Show)

cases :: [TestCase]
cases =
  [ ]
