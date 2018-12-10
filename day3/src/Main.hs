{-# LANGUAGE NamedFieldPuns #-}

module Main where

import           Data.Foldable              (foldl')
import           Data.List                  (find)
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           Data.Maybe                 (catMaybes, fromJust)
import           Data.Monoid                (Sum, getSum)
import qualified Data.Set                   as Set
import           Data.Void                  (Void)
import           Text.Megaparsec            (Parsec, parseMaybe, single)
import           Text.Megaparsec.Char       (space, string)
import qualified Text.Megaparsec.Char.Lexer as L


main :: IO ()
main = do
  input <- readFile "input.txt"

  let claims               = traverse (parseMaybe claim) . lines $ input
      overlappingPoints    = Map.filter (> 1) . overlap <$> claims
      nonOverlappingPoints = Map.filter (== 1) . overlap <$> claims
  putStrLn $ "Part 1: overlapping sq inches " ++ show (Set.size . Map.keysSet <$> overlappingPoints)

  let nonOverlappingClaim = do
        nonOverlappingPoints' <- nonOverlappingPoints
        claims'               <- claims
        find (\c -> let area = areaCovered c in nonOverlappingPoints' `Map.intersection` area == area)
          claims'
  putStrLn $ "Part 2: non-overlapping claim is " ++ show nonOverlappingClaim

overlap :: [Claim] -> Map Point Int
overlap = foldl' (Map.unionWith (+)) Map.empty . map areaCovered

areaCovered :: Claim -> Map Point Int
areaCovered (Claim { claimId, x, y, width, height }) = Map.fromList $ zip points (repeat 1)
  where points = do
          x' <- [x .. x + width - 1]
          y' <- [y .. y + height - 1]
          pure (x', y')

type Point = (Int, Int)
type ClaimId = Int

data Claim = Claim {
    claimId :: ClaimId
  , x       :: Int
  , y       :: Int
  , width   :: Int
  , height  :: Int
  } deriving (Show)

type Parser = Parsec Void String

claim :: Parser Claim
claim = do
  claimId <- single '#' >> L.decimal
  _       <- space >> single '@' >> space
  x       <- L.decimal
  _       <- single ','
  y       <- L.decimal
  _       <- single ':' >> space
  width   <- L.decimal
  _       <- single 'x'
  height  <- L.decimal
  pure Claim { claimId, x, y, width, height }
