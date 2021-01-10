
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "thingo" $ do
    it " works" $
      length [1,2,3] `shouldBe` 3


