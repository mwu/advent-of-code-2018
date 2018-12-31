module Main where

import           Data.Foldable   (maximumBy)
import           Data.Function   ((&))
import           Data.Map        (Map)
import qualified Data.Map        as Map
import qualified Data.Map.Strict as SMap
import           Data.Ord        (comparing)


main :: IO ()
main = do
  let m    = matrixOf $ powerLevel 9445
      sums = sumsFromZero m

  putStr "Part 1: "
  print $ findMaxFuelCellSize sums 3

  putStr "Part 2: "
  let ((maxX, maxY), _, maxSize) = findMaxFuelCell sums
  print (maxX, maxY, maxSize)

findMaxFuelCell :: SMap.Map Coordinate PowerLevel -> (Coordinate, PowerLevel, Int)
findMaxFuelCell sums = maximumBy (comparing power) maxes
  where
    power (_, p, _)     = p
    maxes               = map (\s -> appendSize s $ findMaxFuelCellSize sums s) sizes
    sizes               = [1..300]
    appendSize s (c, p) = (c, p, s)

findMaxFuelCellSize :: SMap.Map Coordinate PowerLevel -> Int -> (Coordinate, PowerLevel)
findMaxFuelCellSize sums k = maximumBy (comparing snd) $ zip coords' (map powerAt coords')
  where
    coords' = filter (\(x,y) -> x <= 300-k+1 && y <= 300-k+1) coords
    powerAt (x,y) = let bottomRightRect = SMap.findWithDefault 0 (x+k-1, y+k-1) sums
                        bottomLeftRect  = SMap.findWithDefault 0 (x-1,   y+k-1) sums
                        topRightRect    = SMap.findWithDefault 0 (x+k-1, y-1)   sums
                        overlap         = SMap.findWithDefault 0 (x-1,   y-1)   sums
                    in
                      bottomRightRect - bottomLeftRect - topRightRect + overlap

-- |Sum all fuel cell squares
sumsFromZero
  :: Map Coordinate PowerLevel      -- ^ All fuel cell power levels
  -> SMap.Map Coordinate PowerLevel -- ^ Map of bottom right point to power levels of square defined from (0,0) to bottom right
sumsFromZero = SMap.foldlWithKey sumFromZero SMap.empty
  where
    sumFromZero m c power = let s = sum' m c power
                            in SMap.insert c s m
    sum' m (x,y) power =
      let rectangleLeftByOneColumn = SMap.findWithDefault 0 (x,   y-1) m
          rectangleAboveByOneRow   = SMap.findWithDefault 0 (x-1, y)   m
          bottomRightPoint         = power
          overlapOfTwoRectangles   = SMap.findWithDefault 0 (x-1, y-1) m
      in
        rectangleLeftByOneColumn + rectangleAboveByOneRow + bottomRightPoint - overlapOfTwoRectangles

-- |Compute the power level of a fuel cell at coordinate (x, y)
-- |in a grid with serial gridSerial.
--
-- Examples:
-- >>> powerLevel 8 (3,5)
-- 4
-- >>> powerLevel 57 (122,79)
-- -5
-- >>> powerLevel 39 (217,196)
-- 0
-- >>> powerLevel 71 (101,153)
-- 4
powerLevel :: GridSerialNumber -> Coordinate -> PowerLevel
powerLevel gridSerial (x, y) =
    rackId * y
  & (+ gridSerial)
  & (* rackId)
  & hundredsDigit
  & (subtract 5)
  where
    rackId          = x + 10
    hundredsDigit n = n `div` 100 `mod` 10

matrixOf :: (Coordinate -> a) -> Map Coordinate a
matrixOf f = Map.fromAscList $ zip coords (map f coords)

coords :: [Coordinate]
coords = do
  j <- [1..300]
  k <- [1..300]
  pure (j,k)

type Coordinate       = (Int, Int)
type PowerLevel       = Int
type GridSerialNumber = Int
