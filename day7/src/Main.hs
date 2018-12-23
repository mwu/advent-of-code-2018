module Main where

import           Data.Char            (chr, ord)
import           Data.Graph.Inductive (Context, Gr, LEdge, Node, context,
                                       delNodes, indeg, isEmpty, lab', match,
                                       mkGraph, nodes, pre, suc')
import           Data.List            (nub, partition, sort, union, (\\))
import           Data.Maybe           (fromJust)
import           Data.Void            (Void)
import           Text.Megaparsec      (Parsec, anySingle, between, parseMaybe)
import           Text.Megaparsec.Char (string)


main :: IO ()
main = do
  input <- lines <$> readFile "input.txt"
  let edges           = map (fromJust . parseMaybe readEdge) input
      (tos, froms, _) = unzip3 edges
      vertices        = nub $ tos ++ froms
      labeledVertices = zip vertices (map chr vertices)
      graph           = mkGraph labeledVertices edges :: Gr Char String
      roots'          = roots graph

  putStr "Part 1: "
  print $ work (head roots') graph (tail roots')

  putStr "Part 2: "
  print $ timeWorked 5 [] graph roots'

  where
    work :: Node -> Gr Char String -> [Node] -> String
    work here graph q =
      case match here graph of
        (Just ctx, graph') -> case sort (q ++ nextCandidates graph' ctx) of
                                []          -> [lab' ctx]
                                (next : q') -> lab' ctx : work next graph' q'
        (Nothing, _)       -> []

    timeWorked :: Int -> [(Node, Int)] -> Gr Char String -> [Node] -> Int
    timeWorked _ _ graph _ | isEmpty graph = 0
    timeWorked numWorkers wip graph q =
      case tick wip of
        (done, inProgress) -> let doneNodes   = map fst done
                                  graph'      = delNodes doneNodes graph
                                  afterDone   = concatMap (nextCandidates graph' . context graph) doneNodes
                                  next        = sort ((q \\ doneNodes) `union` afterDone)
                                  (todo, q')  = splitAt (numWorkers - (length inProgress)) next
                                  inProgress' = inProgress ++ zipBy workFor todo
                              in
                                if null inProgress'
                                then timeWorked numWorkers inProgress' graph' q'
                                else 1 + timeWorked numWorkers inProgress' graph' q'

    nextCandidates :: Gr Char String -> Context Char String -> [Node]
    nextCandidates graph delCtx = filter (null . pre graph) $ suc' delCtx

    tick :: [(Node, Int)] -> ([(Node, Int)], [(Node, Int)]) 
    tick = partition ((== 0) . snd) . map (fmap (subtract 1))

    roots graph = sort $ filter ((== 0) . indeg graph) (nodes graph)

workFor :: Node -> Int
workFor = subtract 4  -- equivalent to 60 + letter position in alphabet

zipBy :: (a -> b) -> [a] -> [(a, b)]
zipBy f xs = zip xs (map f xs)

readEdge :: Parser (LEdge String)
readEdge = do
  from <- string "Step " *> node
  to   <- between (string " must be finished before step ")
                  (string " can begin.")
                  node
  pure (from, to, [chr from, chr to])
  where
    node = ord <$> anySingle

type Parser = Parsec Void String
