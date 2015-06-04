module HgtReader where

import qualified Data.ByteString as S
import qualified Data.Set as Set
import Text.Printf

type Coords = (Double, Double)
type ElevatedPoint = (Coords, Maybe Int)

mkUniq :: Ord a => [a] -> [a]
mkUniq = Set.toList . Set.fromList

truncLatLon :: RealFrac a => a -> Int
truncLatLon l = abs (floor l) :: Int

quadrant :: Coords -> String
quadrant (latitude, longitude) = 
	printf "%s%02d%s%03d" y truncLat x truncLon
	where
		truncLat = truncLatLon latitude
		truncLon = truncLatLon longitude
		x = if longitude > 0 then "E" else "W"
		y = if latitude > 0 then "N" else "S"

quadrants :: [Coords] -> [String]
quadrants = mkUniq . map quadrant

prepare :: [Coords] -> IO [ElevatedPoint]
prepare l = return $ map (\x -> (x, Nothing)) l

elevationAtCellIndex :: S.ByteString -> Int -> Int
elevationAtCellIndex ele n = 
	firstByte * 256 + secondByte
	where
		firstByte = fromIntegral (S.head . S.drop (n*2) $ ele) :: Int
		secondByte = fromIntegral (S.head . S.drop (n*2+1) $ ele) :: Int

elevationInQuadrant :: S.ByteString -> Int -> ElevatedPoint -> ElevatedPoint
elevationInQuadrant ele quadrantSize (coords, Nothing) = 
	(coords, Just (elevationAtCellIndex ele index))
	where
		index = cellIndex quadrantSize coords
elevationInQuadrant _ _ point = point

elevationsInQuadrant :: String -> String -> Int -> IO [ElevatedPoint] -> IO [ElevatedPoint]
elevationsInQuadrant path q quadrantSize points = do
	let file = path ++ "/" ++ q ++ ".hgt"
	elevationData <- S.readFile file
	p <- points
	return $ map (elevationInQuadrant elevationData quadrantSize) p

cellIndex :: Int -> Coords -> Int
cellIndex quadrantSize (latitude, longitude) = 
	round ((1.0 - y) * (q - 1)) * quadrantSize +
	round (x * (q - 1))
	where
		q = fromIntegral quadrantSize
		truncLat = fromIntegral . truncLatLon $ latitude
		truncLon = fromIntegral . truncLatLon $ longitude
		x = longitude - truncLon
		y = latitude - truncLat

elevations :: String -> Int -> [Coords] -> IO [ElevatedPoint]
elevations path quadrantSize l = 
	foldl (\acc x -> elevationsInQuadrant path x quadrantSize acc) (prepare l) (quadrants l)

