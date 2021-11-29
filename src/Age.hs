module Age
  ( ageAtYearEnd,
  )
where

import CommonTypes (BirthDate, Year)
import Data.Time (toGregorian)
import Prelude

ageAtYearEnd :: Year -> BirthDate -> Integer
ageAtYearEnd year birthDate =
  let (birthYear, _, _) = toGregorian birthDate
   in year - birthYear