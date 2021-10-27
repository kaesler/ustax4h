module Kevin
  ( birthDate,
    personalExemptions
  )
where

import CommonTypes
import Data.Time (Day, fromGregorian, toGregorian)

birthDate :: BirthDate
birthDate = fromGregorian 1955 10 2

personalExemptions :: Int 
personalExemptions = 2
