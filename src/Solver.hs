module Solver where
import Data.Graph.Inductive.Graph hiding (isEmpty)
import Data.Graph.Inductive.Internal.Heap
import Control.Monad
import Data.List.Split (splitOn)
import Debug.Trace

type Solver gr w = Node -> Node -> gr () w -> Path

graphFromStr :: (Graph gr, Read w, Num w, Ord w) => String ->  gr () w
graphFromStr filecontents =
  graph
  where
    lined = lines filecontents
    body = map (map read . splitOn " ") . tail $ lined
    nodecount = flip (-) 1 . length $ filecontents
    edges = [(i :: Node, j :: Node, cost) | (i, row) <- zip [1..] body, (j, cost) <- zip [1..] row, cost > 0]
    graph = mkGraph (zip [1..nodecount] (repeat ())) edges

data Algorithm = Uniform | Astar | NoAlg

type Query = (Algorithm, Node, Node)
queryFromStr :: String -> Query
queryFromStr row =
  (alg, start, end)
    where
      [algS, startS, endS] = splitOn " " row
      alg
        | algS == "Uniform" = Uniform
        | algS == "A*" = Astar
        | otherwise = NoAlg
      start = read startS
      end = read endS

query :: (Graph gr) => Query -> gr () Float -> Path
query (alg, initial, goal) = solver initial goal
  where
    solver = case alg of
      Uniform -> uniform
      Astar -> astar
      NoAlg -> \_ _ _ -> []

astarWithHeuristic :: (Graph gr) => (Node -> Node -> gr () Float -> Float) -> Solver gr Float
astarWithHeuristic heuristic initial goal graph =
  inner (unit 0 (0, initial :: Node, []))
  where
    inner heap | isEmpty heap = []
      | node == goal = node:path
      | otherwise = case match node graph of
        (Just (_, _, (), from), g') -> inner . mergeAll . (h':) . map (
            \(nodeCost, n) -> unit (
                cost + nodeCost + heuristic initial goal graph
              ) (cost+nodeCost, n, node:path)
          ) $ from
        (Nothing, g') -> node:path
      where
        (hcost, (cost, node, path), h') = splitMin heap

myHeuristic :: (Graph gr) => Node -> Node -> gr () Float -> Float
myHeuristic current goal graph = case match current graph of
  (Just (_, _, (), from), _) -> minimum . map fst $ from
  (Nothing, _) -> 0

astar :: (Graph gr) => Solver gr Float
astar = astarWithHeuristic myHeuristic

uniform :: (Graph gr) => Solver gr Float
uniform = astarWithHeuristic (\ _ _ _ -> 0)


bfs :: Graph gr => Node -> gr () ()