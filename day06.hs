import Control.Arrow
import Data.Set qualified as Set

main :: IO ()
main = interact $ parse >>> solve1 &&& solve2 >>> \(ans1, ans2) -> (show ans1) ++ "\n" ++ (show ans2) ++ "\n"

type Input = (Grid, Position)

parse :: String -> Input
parse = lines >>> id &&& guard
  where
    guard grid = go (0, 0)
      where
        go (i, j)
          | grid !! i !! j == '^' = (i, j)
          | otherwise =
              let idx = i * n + j + 1
               in go (idx `div` n, idx `mod` n)

        n = length grid
        m = length (grid !! 0)

solve1 :: Input -> Int
solve1 (grid, guard) = length $ go Set.empty guard U
  where
    go visited (i, j) direction
      | isOutside i j = visited
      | grid !! i !! j == '#' =
          let opposite = oppositeDirection direction
              next = nextDirection direction
           in go visited (nextPosition opposite (i, j)) next
      | otherwise =
          let visited' = Set.insert (i, j) visited
           in go visited' (nextPosition direction (i, j)) direction

    isOutside i j = i < 0 || i >= n || j < 0 || j >= m

    n = length grid
    m = length (grid !! 0)

solve2 :: Input -> Int
solve2 (grid, guard) = length . filter hasCycle . map addObstable $ positions
  where
    positions = [(i, j) | i <- [0 .. n - 1], j <- [0 .. m - 1], (i, j) /= guard]

    addObstable (i, j) = swapNth i (swapNth j '#' $ grid !! i) $ grid

    swapNth n x xs = splitAt n >>> second (tail >>> (x :)) >>> uncurry (++) $ xs

    hasCycle grid = go Set.empty guard U
      where
        go visited (i, j) direction
          | isOutside i j = False
          | Set.member (i, j, direction) visited = True
          | grid !! i !! j == '#' =
              let opposite = oppositeDirection direction
                  next = nextDirection direction
               in go visited (nextPosition opposite (i, j)) next
          | otherwise =
              let visited' = Set.insert (i, j, direction) visited
               in go visited' (nextPosition direction (i, j)) direction

    isOutside i j = i < 0 || i >= n || j < 0 || j >= m

    n = length grid
    m = length (grid !! 0)

type Grid = [[Char]]

type Position = (Int, Int)

data Direction = U | D | L | R deriving (Eq, Ord)

oppositeDirection :: Direction -> Direction
oppositeDirection U = D
oppositeDirection D = U
oppositeDirection L = R
oppositeDirection R = L

nextDirection :: Direction -> Direction
nextDirection U = R
nextDirection R = D
nextDirection D = L
nextDirection L = U

nextPosition :: Direction -> Position -> Position
nextPosition U (i, j) = (i - 1, j)
nextPosition D (i, j) = (i + 1, j)
nextPosition L (i, j) = (i, j - 1)
nextPosition R (i, j) = (i, j + 1)
