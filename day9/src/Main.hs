{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections  #-}

module Main where

import           Data.Char                    (isDigit)
import           Data.Foldable                (maximum)
import           Data.Function                ((&))
import           Data.Functor                 ((<&>))
import           Data.List.NonEmpty           (NonEmpty (..))
import qualified Data.ListZipper              as LZ
import           Data.Map                     (Map)
import qualified Data.Map                     as Map
import           Text.ParserCombinators.ReadP (between, munch1, readP_to_S,
                                               string)


main :: IO ()
main = do
  gameSetup <- read <$> readFile "input.txt"

  putStr "Part 1: "
  print $ winningScore <$> playMarbles gameSetup

  putStr "Part 2: "
  let gameSetup2 = gameSetup { numMarbles  = 100 * numMarbles gameSetup }
  print $ winningScore <$> playMarbles gameSetup2

playMarbles :: GameSetup -> Maybe GameState
playMarbles GameSetup { numPlayers, numMarbles } = play playerTurns marbles start
  where
    start       = (LZ.zipper0L' (0 :| []), Map.empty)
    playerTurns = cycle [1..numPlayers]
    marbles     = [1..numMarbles]

    play :: [Int] -> [Int] -> GameState -> Maybe GameState
    play _       []    state                         = Just state
    play []      _     _                             = Nothing
    play (p:ps) (m:ms) (z, scores) | m `mod` 23 == 0 = iterate LZ.moveLeftLoop z !! 7
                                                       & LZ.runListZipperOp (LZ.deleteStepRight
                                                                             <&> (\x -> Map.insertWith (+) p (x + m) scores))
                                                       >>= play ps ms
    play (_:ps) (m:ms) (z, scores)                   = LZ.moveRightLoop z
                                                       & LZ.insertMoveLeft m
                                                       & play ps ms . (, scores)

winningScore :: GameState -> Int
winningScore = maximum . Map.elems . snd

data GameSetup = GameSetup {
  numPlayers :: Int
, numMarbles :: Int
}

instance Read GameSetup where
  readsPrec _ = readP_to_S setup
    where
      setup = GameSetup
              <$> int
              <*> between (string " players; last marble is worth ")
                          (string " points")
                          int
      int   = read <$> munch1 isDigit

type GameState = (LZ.ListZipper Int, ScoreCard)
type ScoreCard = Map Player Int
type Player    = Int
type Score     = Int
