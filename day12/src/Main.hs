{-# LANGUAGE NamedFieldPuns #-}

module Main where

import           Control.Applicative  ((<|>))
import           Data.Foldable        (toList)
import qualified Data.Map             as Map
import           Data.Sequence        (Seq (..), (><))
import qualified Data.Sequence        as Seq
import           Data.Void            (Void)
import           Text.Megaparsec      (Parsec, manyTill, parseMaybe, sepEndBy1)
import           Text.Megaparsec.Char (char, space1, string)


main :: IO ()
main = do
  w <- parseMaybe world <$> readFile "input.txt"

  let generation20 = generation 20 <$> w
  putStr "Part 1: "
  print $ sumLivePlantPots <$> generation20

  let generation50billion = generation 50000000000 <$> w
  putStr "Part 2: "
  print $ sumLivePlantPots <$> generation50billion

sumLivePlantPots :: World -> Int
sumLivePlantPots = sum
                   . map fst
                   . filter ((== Alive) . snd)
                   . indexedPlants
  where
    indexedPlants World { start, plants } = zip [start..] $ toList plants

-- step n generations with two optimizations: memoization and stable state detection
generation :: Int -> World -> World
generation = memoize Map.empty
  where
    memoize _ 0 w = w
    memoize m n w = case Map.lookup (plants w) m of
                      Just w' -> memoize m (n - 1) w'
                      Nothing -> let v = step w
                                 in
                                   if plants v == plants w
                                   then
                                     -- stable but moving, calculate the final starting position
                                     v { start = start v - (start w - start v) * (n - 1) }
                                   else
                                     memoize (Map.insert (plants w) v m) (n - 1) v

step :: World -> World
step world' = shrinkWorld $ applyRules world'
  where
    stepPlants ps = Map.findWithDefault Dead ps allRules
    allRules      = Map.fromList $ rules world'

    applyRules w = let w' = extendWorld w
                   in
                     w' {
                       start  = start w' + 2 -- 4 extra dead on the left == 2 extra combination of fives
                     , plants = mapFives stepPlants $ plants w'
                     }

    -- extend on each side by 4 dead in case there's ....# or #.... patterns
    extendWorld w = let fourDead = Seq.replicate 4 Dead
                    in
                      w { start  = start w - 4
                        , plants = fourDead >< plants w >< fourDead }

    shrinkWorld w@(World { start, plants }) =
      case plants of
        (Dead :<| m) :|> Dead -> shrinkWorld (w { start = start + 1, plants = m })
        Dead :<| m            -> shrinkWorld (w { start = start + 1, plants = m })
        m :|> Dead            -> shrinkWorld (w { plants = m })
        _                     -> w

mapFives :: ((e, e, e, e, e) -> a) -> Seq e -> Seq a
mapFives _ (_ :<| _ :<| _ :<| _ :<| Seq.Empty)  = Seq.Empty
mapFives f (a :<| r@(b :<| c:<| d :<| e :<| _)) = f (a, b, c, d, e) :<| mapFives f r

world :: Parser World
world = do
  plants <- Seq.fromList <$> (string "initial state: " *> plant `manyTill` space1)
  rules  <- rule `sepEndBy1` space1
  pure World { start = 0, plants, rules }

rule :: Parser Rule
rule = do
  left  <- (,,,,) <$> plant <*> plant <*> plant <*> plant <*> plant
  _     <- string " => "
  right <- plant
  pure (left, right)

plant :: Parser Plant
plant = dead <|> alive
  where
    dead  = char '.' *> pure Dead
    alive = char '#' *> pure Alive


data World = World {
  start  :: Int
, plants :: Seq Plant
, rules  :: [Rule]
} deriving (Show, Eq)

data Plant = Dead | Alive deriving (Show, Eq, Ord)

type Rule = ((Plant, Plant, Plant, Plant, Plant), Plant)
type Parser = Parsec Void String
