{-# LANGUAGE NamedFieldPuns #-}

module Main where

import           Data.Bifunctor (first)
import           System.Exit    (die)


main :: IO ()
main = do
  input <- map read . words <$> readFile "input.txt"
  case toTree input of
    Nothing        -> die "Unable to parse input into tree."
    Just (tree, _) -> do
      putStr "Part 1: "
      print $ sumMetadata tree

      putStrLn $ "Part 2: " ++ show (value tree)

toTree :: [Int] -> Maybe (Tree Int, [Int])
toTree (numChildren : numMetadata : xs) = do
  (children, xs') <- toLeaves numChildren xs
  let (metadata, rest) = splitAt numMetadata xs'
      value'           = if null children
                         then sum metadata
                         else sum $ map (maybe 0 value . elemAt children) metadata
  Just (TreeNode { value = value', children, metadata }, rest)

toTree _ = Nothing

toLeaves :: Int -> [Int] -> Maybe ([Tree Int], [Int])
toLeaves 0 xs = Just ([], xs)
toLeaves n xs = do
  (t, rest) <- toTree xs
  first (t :) <$> toLeaves (n - 1) rest

sumMetadata :: Tree Int -> Int
sumMetadata TreeNode { children, metadata } = sum metadata + sum (map sumMetadata children)

elemAt :: [a] -> Int -> Maybe a
elemAt []     _ = Nothing
elemAt (x:_)  1 = Just x
elemAt (_:xs) n = elemAt xs (n - 1)


data Tree a = TreeNode {
  value    :: a
, children :: [Tree a]
, metadata :: [a]
} deriving (Show)
