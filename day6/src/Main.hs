{-# LANGUAGE NamedFieldPuns #-}

module Main where

import           Data.Char                    (isDigit)
import           Data.Foldable                (any)
import           Data.List                    (groupBy, sortOn)
import           Data.Maybe                   (isJust)
import           Text.ParserCombinators.ReadP (munch1, readP_to_S, string)


main :: IO ()
main = do
  input <- lines <$> readFile "input.txt"
  let grid = determineGrid $ map read input

  putStr "Part 1: "
  print
    . maximum
    . map length
    . filter (not . flip touchesEdgesOf grid)
    . map (map fst)
    . groupBy (\x y -> snd x == snd y)
    . sortOn snd
    . filter (isJust . snd)
    . gridToProximityMap
    $ grid

  putStr "Part 2: "
  print
    . length
    . filter (\c -> sum (map (distance c) (coords grid)) < 10000)
    . coordsInGrid
    $ grid

touchesEdgesOf :: [Coordinate] -> PartialGrid -> Bool
touchesEdgesOf cs (PartialGrid { top, right, bottom, left }) =
  any (\(Coordinate (x, y)) -> top == y || bottom == y || right == x || left == x) cs

gridToProximityMap :: PartialGrid -> [(Coordinate, Maybe Coordinate)]
gridToProximityMap g@(PartialGrid { coords }) =
  map (\c -> (c, closestCoord c)) (coordsInGrid g)
  where
    closestCoord  c = let cs = closestCoords c in
                        if length cs > 1
                        then Nothing
                        else Just $ head cs
    closestCoords c = map fst
                         $ head
                         $ groupBy (\x y -> snd x == snd y)
                         $ sortOn snd
                         $ zip coords (map (distance c) coords)

coordsInGrid :: PartialGrid -> [(Coordinate)]
coordsInGrid PartialGrid { top, right, bottom, left } = do
  x <- [ left .. right ]
  y <- [ top .. bottom ]
  pure $ Coordinate (x, y)

determineGrid :: [Coordinate] -> PartialGrid
determineGrid []       = singletonGrid $ Coordinate (0,0)
determineGrid (c : cs) = foldr enlargeBy (singletonGrid c) cs

singletonGrid :: Coordinate -> PartialGrid
singletonGrid c@(Coordinate (x, y)) = PartialGrid {
    top    = y
  , right  = x
  , bottom = y
  , left   = x
  , coords = [c]
  }

enlargeBy :: Coordinate -> PartialGrid -> PartialGrid
enlargeBy c@(Coordinate (x,y)) (PartialGrid { top, right, bottom, left, coords }) =
  PartialGrid {
    top    = min top y
  , right  = max right x
  , bottom = max bottom y
  , left   = min left x
  , coords = c : coords
  }

distance :: Coordinate -> Coordinate -> Int
distance (Coordinate (p1,p2)) (Coordinate (q1,q2)) = abs (p1 - q1) + abs (p2 - q2)


data PartialGrid = PartialGrid {
    top    :: Int
  , right  :: Int
  , bottom :: Int
  , left   :: Int
  , coords :: [Coordinate]
  } deriving (Show)

newtype Coordinate = Coordinate (Int, Int)
  deriving (Show, Eq, Ord)

instance Read Coordinate where
  readsPrec _ = readP_to_S $ Coordinate <$> pair
    where
      pair = (,) <$> int
                 <*> (string ", " *> int)
      int  = read <$> munch1 isDigit
