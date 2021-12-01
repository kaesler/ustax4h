module AgeSpec
  ( ageSpec,
  )
where

import Age
import Data.Time (fromGregorian)
import Test.Hspec
  ( SpecWith,
    describe,
    it,
    shouldBe,
  )
import Prelude

ageSpec :: SpecWith ()
ageSpec =
  describe "Age.isAge65OrOlder" $
    it "works as expected" $ do
      Age.isAge65OrOlder (fromGregorian 1955 10 2) 2019 `shouldBe` False
      Age.isAge65OrOlder (fromGregorian 1955 10 2) 2020 `shouldBe` True
      Age.isAge65OrOlder (fromGregorian 1956 1 1) 2020 `shouldBe` True
