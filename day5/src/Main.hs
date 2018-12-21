module Main where

import           Control.Applicative ((<|>))
import           Data.Char           (toLower, toUpper)
import           Data.Foldable       (minimum)
import           Data.Functor        (void)
import qualified Data.ListZipper     as LZ
import           Data.Maybe          (fromMaybe)


main :: IO ()
main = do
  input <- head . lines <$> readFile "input.txt"
  putStrLn $ "Part 1: " ++ (show $ part1 input)
  putStrLn $ "Part 2: " ++ (show $ part2 input)
  where
    part1           = length . reactPolymer
    part2           = minimum
                      . map (part1 . uncurry removeUnit)
                      . zip ['a'..'z']
                      . repeat
    removeUnit unit = filter (\x -> x /= toLower unit && x /= toUpper unit)

reactPolymer :: String -> String
reactPolymer s = fromMaybe "" $ LZ.list <$> LZ.opWhileJust react <$> LZ.zipper s
  where react = do
          c <- LZ.getFocus
          r <- LZ.getRight
          if isOpposites c r
            then
              LZ.deleteStepRight
              *> (LZ.deleteStepRight <|> LZ.deleteStepLeft) -- don't step off the end
              *> (LZ.moveLeft        <|> void LZ.getFocus)  -- don't step past the start
            else LZ.moveRight

isOpposites :: Char -> Char -> Bool
isOpposites x y = x /= y && toLower x == toLower y

