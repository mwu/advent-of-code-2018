{-# LANGUAGE NamedFieldPuns #-}

module Main where

import           Control.Monad              (forM_)
import           Data.Bifunctor             (bimap)
import           Data.Function              (on)
import           Data.List                  (group, groupBy, sort, sortOn)
import           Data.Maybe                 (fromJust)
import           Data.Semigroup             (Max (..), Min (..))
import qualified Data.Set                   as Set
import           Data.Void                  (Void)
import           Text.Megaparsec            (Parsec, between, parseMaybe)
import           Text.Megaparsec.Char       (char, space, string)
import qualified Text.Megaparsec.Char.Lexer as L


main :: IO ()
main = do
  input <- lines <$> readFile "input.txt"
  let stars           = fromJust $ traverse (parseMaybe star) input
      frames          = iterate (map move) stars
      iFrames         = zip frames ([0..] :: [Int])
      (message, time) = filter (likelyMessage . fst) iFrames !! 0

  putStrLn "Part 1: "
  render message

  putStrLn $ "Part 2: " ++ show time

-- this is a shot in the dark that a message produces more than
-- 5 lines of length 2 because of the shape of Roman letters
likelyMessage :: [Star] -> Bool
likelyMessage ss = numHorizLines > 5
  where
    numHorizLines = length $ filter (`hasHorizLineSegmentsOf` 2) xGroup
    xGroup        = groupBy ((==) `on` fst) $ sortOn fst stars
    stars         = map position ss

hasHorizLineSegmentsOf :: [(Int, Int)] -> Int -> Bool
hasHorizLineSegmentsOf ps len = any ((> len) . length) $ group diffs
  where
    diffs = map (abs . uncurry (-)) $ zip ys (tail ys)
    ys = sort $ map snd ps

render :: [Star] -> IO ()
render ss = do
  forM_ [oy..by] $ \y -> do
    forM_ [ox..bx] $ \x -> do
      if Set.member (x,y) starSet
        then putStr "*"
        else putStr " "
    putStr "\n"
  where
    Grid { origin = (ox,oy), bottomRight = (bx,by) } = starGrid ss
    starSet = Set.fromList $ map position ss

starGrid :: [Star] -> Grid
starGrid ss = Grid { origin, bottomRight }
  where
    origin                = bimap getMin getMin top
    bottomRight           = bimap getMax getMax bottom
    (top, bottom)         = foldMap (minAndMaxPoints . position) ss
    minAndMaxPoints (x,y) = ((Min x, Min y), (Max x, Max y))

moveBy :: (Int, Int) -> Star -> Star
moveBy (x, y) s@(Star { position }) = s { position = bimap (+x) (+y) position }

move :: Star -> Star
move s@Star { velocity } = moveBy velocity s

star :: Parser Star
star = do
  position <- taggedPair "position"
  velocity <- taggedPair "velocity"
  pure Star { position, velocity }

taggedPair :: String -> Parser (Int, Int)
taggedPair s = string s *> char '=' *> lexeme bracketedPair

bracketedPair :: Parser (Int, Int)
bracketedPair = between left right pair
  where
    left  = lexeme $ char '<'
    right = lexeme $ char '>'
    pair  = (,) <$> int <* lexeme (char ',') <*> int

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

int :: Parser Int
int = lexeme $ L.signed nothing L.decimal

nothing :: Parser ()
nothing = pure ()

data Grid = Grid {
  origin      :: (Int, Int)
, bottomRight :: (Int, Int)
} deriving (Show)

data Star = Star {
  position :: (Int, Int)
, velocity :: (Int, Int)
} deriving (Show)

type Parser = Parsec Void String
