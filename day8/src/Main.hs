{-# LANGUAGE NamedFieldPuns #-}

module Main where

import           Data.Bifunctor (first)


main :: IO ()
main = do
  input <- map read . words <$> readFile "input.txt"
  let (tree, _) = toTree input

  putStr "Part 1: "
  print $ sumMetadata tree

  putStrLn $ "Part 2: " ++ show (value tree)

toTree :: [Int] -> (Tree Int, [Int])
toTree (numChildren : numMetadata : xs) = (TreeNode { value = value', children, metadata }, rest)
  where
    (children, xs')  = toLeaves numChildren xs
    (metadata, rest) = splitAt numMetadata xs'
    value'           = if null children
                       then sum metadata
                       else sum $ map (maybe 0 value . elemAt children) metadata

toLeaves :: Int -> [Int] -> ([Tree Int], [Int])
toLeaves 0 xs = ([], xs)
toLeaves n xs = first (t :) $ toLeaves (n - 1) rest
  where
    (t, rest) = toTree xs

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
