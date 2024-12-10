import Control.Arrow
import Data.List
import Data.Map qualified as Map

main :: IO ()
main = interact $ parse >>> solve1 &&& solve2 >>> \(ans1, ans2) -> (show ans1) ++ "\n" ++ (show ans2) ++ "\n"

type Input = [[Char]]

parse :: String -> Input
parse = lines

solve1 :: Input -> Int
solve1 grid = length . filter inside $ allAntinodes
  where
    allAntinodes = nub . sort . concatMap (snd >>> antinodesOfMany) . Map.toList $ antennas

    antinodesOfMany [_] = []
    antinodesOfMany (p : ps) = do
      p' <- ps
      antinode p p' : antinode p' p : antinodesOfMany ps

    antinode (x1, y1) (x2, y2) =
      let dx = x2 - x1
          dy = y2 - y1
       in (x2 + dx, y2 + dy)

    -- map of frequency to list of positions
    antennas = go Map.empty 0 0
      where
        go map i j
          | i == n = map
          | j == m = go map (i + 1) 0
          | c == '.' = go map i (j + 1)
          | otherwise =
              let map' = Map.insertWith (++) c [(i, j)] map
               in go map' i (j + 1)
          where
            c = grid !! i !! j

    inside (x, y) = x >= 0 && x < n && y >= 0 && y < m
    n = length grid
    m = length $ grid !! 0

solve2 :: Input -> Int
solve2 grid = length allAntinodes
  where
    allAntinodes = nub . sort . concatMap (snd >>> antinodesOfMany) . Map.toList $ antennas

    antinodesOfMany [_] = []
    antinodesOfMany (p : ps) = do
      p' <- ps
      antinodes p p' ++ antinodes p' p ++ antinodesOfMany ps

    antinodes (x1, y1) (x2, y2) =
      let dx = x2 - x1
          dy = y2 - y1
       in takeWhile inside [(x2 + i * dx, y2 + i * dy) | i <- [0 ..]]

    -- map of frequency to list of positions
    antennas = go Map.empty 0 0
      where
        go map i j
          | i == n = map
          | j == m = go map (i + 1) 0
          | c == '.' = go map i (j + 1)
          | otherwise =
              let map' = Map.insertWith (++) c [(i, j)] map
               in go map' i (j + 1)
          where
            c = grid !! i !! j

    inside (x, y) = x >= 0 && x < n && y >= 0 && y < m
    n = length grid
    m = length $ grid !! 0
