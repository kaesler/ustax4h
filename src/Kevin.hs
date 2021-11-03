module Kevin
  ( birthDate,
    filingStatus,
    personalExemptions,
  )
where

import CommonTypes (BirthDate, FilingStatus (HeadOfHousehold))
import Data.Time (Day, fromGregorian, toGregorian)

birthDate :: BirthDate
birthDate = fromGregorian 1955 10 2

filingStatus :: FilingStatus
filingStatus = HeadOfHousehold

personalExemptions :: Int
personalExemptions = 2
