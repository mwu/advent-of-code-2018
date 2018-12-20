{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import           Control.Applicative        ((<|>))
import           Data.Foldable              (foldl', foldMap, maximumBy)
import           Data.List                  (group, groupBy, sort)
import           Data.List.Split            (chunksOf)
import           Data.Ord                   (comparing)
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           Data.Maybe                 (fromJust)
import           Data.Semigroup             (Sum(..))
import           Data.Time.Clock            (NominalDiffTime)
import           Data.Time.Format           (defaultTimeLocale, parseTimeM)
import           Data.Time.LocalTime        (LocalTime(..), diffLocalTime, todMin)
import           Data.Void                  (Void)
import           Text.Megaparsec            (Parsec, between, parseMaybe, single, takeWhile1P)
import           Text.Megaparsec.Char       (space, string)
import           Text.Megaparsec.Char.Lexer (decimal)


main :: IO ()
main = do
  input <- readFile "input.txt"
  let shifts = recordsToGuardShifts
               $ fromJust
               $ traverse (parseMaybe guardRecord)
               $ lines
               $ input

      sleepTotalByGuardId :: Map GuardId NominalDiffTime
      sleepTotalByGuardId = foldl' (Map.unionWith (+)) Map.empty
                            $ map (\g -> Map.singleton (guardId g) (sleepTotal g)) shifts
      sleepiestGuardId    = fst
                            $ maximumBy (comparing snd)
                            $ Map.toList sleepTotalByGuardId

      sleepHistogramsByGuardId :: Map GuardId (Map Int Int)
      sleepHistogramsByGuardId = foldl' (Map.unionWith $ Map.unionWith (+)) Map.empty
                                 $ map (\g -> Map.singleton (guardId g) (histogram $ sleepToMinutes g))
                                 $ shifts

      sleepiestGuardMaxMinute = fst
                                <$> maximumBy (comparing snd)
                                <$> Map.toList
                                <$> Map.lookup sleepiestGuardId sleepHistogramsByGuardId

      sleepiestGuardIdAndMinute = fmap fst
                                  $ maximumBy (comparing $ snd . snd)
                                  $ Map.toList
                                  $ Map.map (maximumBy (comparing snd) . Map.toList)
                                  $ Map.filter (/= Map.empty)
                                  $ sleepHistogramsByGuardId
      
  putStrLn $ "Part 1: " ++ (show $ (*) <$> Just sleepiestGuardId <*> sleepiestGuardMaxMinute)
  putStrLn $ "Part 2: " ++ (show $ uncurry (*) sleepiestGuardIdAndMinute)

histogram :: [Int] -> Map Int Int
histogram = foldl' (\acc xs -> Map.insert (head xs) (length xs) acc) Map.empty
            . group
            . sort

sleepTotal :: GuardShift -> NominalDiffTime
sleepTotal = getSum . foldMap (Sum . (uncurry $ flip diffLocalTime)) . sleeps

sleepToMinutes :: GuardShift -> [Int]
sleepToMinutes = foldMap minutesBetween . sleeps
  where minutesBetween sleep = [startOf sleep .. endOf sleep]
        startOf              = todMin . localTimeOfDay . fst
        endOf                = (subtract 1) . todMin . localTimeOfDay . snd

recordsToGuardShifts :: [GuardRecord] -> [GuardShift]
recordsToGuardShifts recs = map toGuardShifts
                            . breakOnBeginShifts
                            . sort
                            $ recs
  where breakOnBeginShifts = groupBy (\_ b -> case b of
                                                GuardRecord _ (BeginShift _) -> False
                                                _ -> True)
        toGuardShifts ((GuardRecord startTime (BeginShift guardId)) : sleeps') =
          GuardShift { guardId
                     , startTime
                     , sleeps = map (\[x,y] -> (time x, time y)) $ chunksOf 2 sleeps' }

guardRecord :: Parser GuardRecord
guardRecord = do
  t <- recordLocalTime
  _ <- space
  wakeUp t <|> fallAsleep t <|> beginShift t
  where wakeUp t     = string "wakes up" >> pure (GuardRecord t WakeUp)
        fallAsleep t = string "falls asleep" >> pure (GuardRecord t FallAsleep)
        beginShift t = between (string "Guard #") (string " begins shift") decimal >>=
                       pure . GuardRecord t . BeginShift

recordLocalTime :: Parser LocalTime
recordLocalTime = do
  s <- between (single '[') (single ']') (takeWhile1P (Just "local time") (/= ']'))
  parseTimeM False defaultTimeLocale "%F %R" s


type Parser = Parsec Void String

type GuardId = Int

data GuardShift = GuardShift {
    guardId   :: GuardId
  , startTime :: LocalTime
  , sleeps    :: [(LocalTime, LocalTime)]
  } deriving (Show, Eq)

data GuardRecord = GuardRecord {
    time   :: LocalTime
  , action :: GuardAction
  } deriving (Show, Eq)

data GuardAction = BeginShift GuardId | FallAsleep | WakeUp
  deriving (Show, Eq)

instance Ord GuardRecord where
  compare x y = compare (time x) (time y)
