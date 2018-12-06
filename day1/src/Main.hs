module Main where

import           Conduit
import           Control.Monad.State (evalState, get, modify)
import           Data.Bifunctor      (first)
import           Data.Either         (isRight)
import           Data.Functor        (void)
import           Data.Maybe          (fromJust)
import           Data.Set            (Set)
import qualified Data.Set            as Set
import qualified Data.Text.Lazy      as T
import qualified Data.Text.Lazy.IO   as T
import qualified Data.Text.Lazy.Read as TR
import           Data.Text.Read      (Reader, decimal, signed)


main :: IO ()
main = do
  sum <- partOne
  putStrLn $ "Final frequency is " ++ show sum

  repeat <- partTwo
  putStrLn $ "First repeat is " ++ show repeat

partOne :: IO Int
partOne = runConduitRes
           $ sourceFile "input.txt"
          .| decodeUtf8C
          .| linesUnboundedC
          .| mapC (signed decimal :: Reader Int)
          .| mapCE fst
          .| sumCE

partTwo :: IO (Maybe Int)
partTwo = do
  lines                <- fmap T.lines $ T.readFile "input.txt"
  let frequencyChanges = map (TR.signed TR.decimal :: TR.Reader Int) lines
      firstRepeat      = flip evalState (Set.empty :: Set Int) $ runConduit
                            $ yieldMany (cycle frequencyChanges)
                           .| concatC
                           .| mapC fst
                           .| scanlC (+) 0
                           .| filterMC (\x -> do
                                           seen <- get
                                           if Set.member x seen
                                             then return True
                                             else modify (Set.insert x) >> return False)
                           .| headC
  return firstRepeat
