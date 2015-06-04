import Test.Hspec
import HgtReader

main :: IO ()
main = hspec $
  describe "HgtReader" $ do
    it "determines the quadrant a coordinate point belongs in" $ do
      quadrant (62.3, 10.1) `shouldBe` "N62E010"
      quadrant (62.0, 10.0) `shouldBe` "N62E010"
      quadrant (62.3, -10.1) `shouldBe` "N62W011"
      quadrant (-62.3, -10.1) `shouldBe` "S63W011"

    it "determines the quadrants for a list of coordinates" $
      quadrants
	[(62, 10.1), 
	 (62.1, 10), 
	 (63.0, 10)]
	`shouldBe` ["N62E010", "N63E010"]

    it "determines the cell index of a coordinate point inside a quadrant" $ do
      cellIndex 3 (62.9, 10.0) `shouldBe` 0
      cellIndex 3 (62.9, 10.9) `shouldBe` 2
      cellIndex 3 (62.5, 10.5) `shouldBe` 4
      cellIndex 3 (62.0, 10.0) `shouldBe` 6
      cellIndex 3 (62.0, 10.9) `shouldBe` 8
      cellIndex 3 (62.24, 10.0) `shouldBe` 6
      cellIndex 3 (62.26, 10.0) `shouldBe` 3
      cellIndex 3601 (62.0, 10.99999) `shouldBe` 3601 * 3601 - 1

    it "determines elevations for a list of coordinates" $ do
      calc <- elevations "elevation" 3601
	[(62.0, 10.0), 
	 (62.0, 10.9999), 
	 (62.9999, 10.0),
 	 (62.64706079, 10.33255505),
	 (62.64164653, 10.36175275)]
      calc
	`shouldBe` 
	[((62.0, 10.0), Just 800), 
	 ((62.0, 10.9999), Just 895), 
	 ((62.9999, 10.0), Just 539),
	 ((62.64706079, 10.33255505), Just 888),
	 ((62.64164653, 10.36175275), Just 1240)]

