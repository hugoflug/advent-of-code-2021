import Test.Hspec
import qualified AOC1_1
import qualified AOC1_2

goldStar :: (HasCallStack, Example a) => a -> SpecWith (Arg a)
goldStar = specify "GOLD STAR *"

main :: IO ()
main = hspec $ do
  describe "1_1" $ do
    goldStar $ do
      input <- readFile "test/AOC1.txt"
      AOC1_1.solve input `shouldBe` 1722
  describe "1_2" $ do
    goldStar $ do
      input <- readFile "test/AOC1.txt"
      AOC1_2.solve input `shouldBe` 1748
