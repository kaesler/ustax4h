module TestDataFromScala where

import Taxes
    ( FilingStatus(..),
      QualifiedIncome,
      SocSec,
      TaxableOrdinaryIncome )

cases :: [(FilingStatus, SocSec, TaxableOrdinaryIncome,
  QualifiedIncome, Double)]
cases = fmap ingestCase [
  (HeadOfHousehold,47145,0,1,0),
  (Single,22782,0,47408,2417),
  (HeadOfHousehold,25091,0,22047,0),
  (HeadOfHousehold,0,0,0,0)
  ]
ingestCase :: (FilingStatus, Integer, Integer, Integer, Integer) -> 
  (FilingStatus, SocSec, TaxableOrdinaryIncome, QualifiedIncome, Double)
ingestCase (filingStatus, ss, inc, qi, expectedTax) = (
    filingStatus,
    fromInteger ss,
    fromInteger inc,
    fromInteger qi,
    fromInteger expectedTax
  )