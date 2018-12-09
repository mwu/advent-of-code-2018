module Main where

import           Control.Monad  (join)
import           Data.Bifunctor (bimap)
import           Data.List      (find, group, nub, partition, sort)
import           Data.Maybe     (fromJust)

main :: IO ()
main = do
  partOne <- checksum <$> readFile "input.txt"
  putStrLn $ "The checksum is " ++ show partOne

  partTwo <- findCommonChars . lines <$> readFile "input.txt"
  putStrLn $ "The common chars are " ++ fromJust partTwo

checksum :: String -> Int
checksum = uncurry (*)
           . join bimap length
           . partition (== 2)
           . concatMap (nub . filter (\x -> x == 2 || x == 3) . duplicateCharCounts)
           . lines
  where duplicateCharCounts :: String -> [Int]
        duplicateCharCounts = map length . group . sort

findCommonChars :: [String] -> Maybe String
findCommonChars []     = Nothing
findCommonChars (x:xs) = maybe (findCommonChars xs) Just (findCommon x xs)
  where findCommon :: String -> [String] -> Maybe String
        findCommon s = fmap fst
                       . find ((== 1) . length . snd)
                       . map (commonChars s)

commonChars :: String -> String -> (String, String)
commonChars s1 s2 = join bimap (map fst)
                    $ partition (uncurry (==))
                    $ zip s1 s2
