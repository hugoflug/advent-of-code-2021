import Test.Hspec
import qualified AOC1_1
import qualified AOC1_2
import qualified AOC2_1
import qualified AOC2_2
import qualified AOC3_1
import qualified AOC3_2

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
  describe "2_1" $ do
    goldStar $ do
      input <- readFile "test/AOC2.txt"
      AOC2_1.solve input `shouldBe` 1654760
  describe "2_2" $ do
    goldStar $ do
      input <- readFile "test/AOC2.txt"
      AOC2_2.solve input `shouldBe` 1956047400
  describe "3_1" $ do
    goldStar $ do
      input <- readFile "test/AOC3.txt"
      AOC3_1.solve input `shouldBe` 1458194
  describe "3_2" $ do
    goldStar $ do
      input <- readFile "test/AOC3.txt"
      AOC3_2.solve input `shouldBe` 2829354