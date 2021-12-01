module Age
  ( isAge65OrOlder,
  )
where

import CommonTypes (BirthDate, Year)
import Data.Time (addGregorianYearsClip, fromGregorian, toGregorian)
import Prelude

-- You're considered by the IRS to be 65 on the day before your
-- 65th birthday. Therefore, you are considered age 65 at the
-- end of the year if your 65th birthday is on or before
-- January 1 of the following year.
-- TODO: unit test it.
isAge65OrOlder :: BirthDate -> Year -> Bool
isAge65OrOlder bd y =
  let firstDayAfterYearEnd = fromGregorian (succ y) 1 1
   in bd <= addGregorianYearsClip (-65) firstDayAfterYearEnd