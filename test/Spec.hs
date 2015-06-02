import Test.Hspec
import HgtReader

main :: IO ()
main = hspec $ do
  describe "HgtReader" $ do
    it "determines the quadrant a coordinate point belongs in" $ do
      quadrant (62.3, 10.1) `shouldBe` "N62E010"
      quadrant (62.0, 10.0) `shouldBe` "N62E010"
      quadrant (62.3, -10.1) `shouldBe` "N62W011"
      quadrant (-62.3, -10.1) `shouldBe` "S63W011"

    it "determines the quadrants for a list of coordinates" $ do
      quadrants
	[(62, 10.1), 
	 (62.1, 10), 
	 (63.0, 10)]
	`shouldBe` ["N62E010", "N63E010"]

    it "determines elevations for a list of coordinates" $ do
      calc <- elevations "elevation"
	[(62, 10.1), 
	 (62.1, 10), 
	 (63.0, 10)]
      calc
	`shouldBe` 
	[((62.0, 10.1), Just 133), 
	 ((62.1, 10.0), Just 13), 
	 ((63.0, 10.0), Just 1)]

