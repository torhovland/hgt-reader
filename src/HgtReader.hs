module HgtReader where

import qualified Data.ByteString as S
import Control.Monad
import qualified Data.Set as Set
import Text.Printf

type Coords = (Double, Double)
type ElevatedPoint = (Coords, Maybe Integer)

mkUniq :: Ord a => [a] -> [a]
mkUniq = Set.toList . Set.fromList

quadrant :: Coords -> String
quadrant (latitude, longitude) = 
	printf "%s%02d%s%03d" x truncLat y truncLon
	where
		truncLat = (abs (floor latitude)) :: Int
		truncLon = (abs (floor longitude)) :: Int
		x = (if latitude > 0 then "N" else "S")
		y = (if longitude > 0 then "E" else "W")

quadrants :: [Coords] -> [String]
quadrants = mkUniq . map quadrant

prepare :: [Coords] -> IO [ElevatedPoint]
prepare l = do
	return $ map (\x -> (x, Nothing)) l

elevationInQuadrant :: S.ByteString -> ElevatedPoint -> ElevatedPoint
elevationInQuadrant ele ((latitude, longitude), Nothing) = ((latitude, longitude), Just 124)
elevationInQuadrant _ point = point

elevationsInQuadrant :: String -> String -> IO [ElevatedPoint] -> IO [ElevatedPoint]
elevationsInQuadrant path q points = do
	let file = path ++ "/" ++ q ++ ".hgt"
	elevationData <- S.readFile file
	p <- points
	return $ map (elevationInQuadrant elevationData) p

elevations :: String -> [Coords] -> IO [ElevatedPoint]
elevations path l = foldl (\acc x -> elevationsInQuadrant path x acc) (prepare l) (quadrants l)

