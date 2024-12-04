import Control.Arrow

main :: IO ()
main = interact $ parse >>> solve1 &&& solve2 >>> \(ans1, ans2) -> (show ans1) ++ "\n" ++ (show ans2) ++ "\n"

type Input = [[Char]]

parse :: String -> Input
parse = lines

solve1 :: Input -> Int
solve1 input = length $ filter xmas [(pos, dir) | pos <- allPositions, dir <- allDirections]
  where
    xmas ((i, j), (u, v))
      | isBetween 0 n (i + 3 * u) && isBetween 0 m (j + 3 * v) =
          all
            isCharAt
            [ (i + 0 * u, j + 0 * v, 'X'),
              (i + 1 * u, j + 1 * v, 'M'),
              (i + 2 * u, j + 2 * v, 'A'),
              (i + 3 * u, j + 3 * v, 'S')
            ]
      | otherwise = False

    isBetween a b x = x >= a && x < b

    isCharAt (i, j, c) = input !! i !! j == c

    allPositions = [(i, j) | i <- [0 .. n - 1], j <- [0 .. m - 1]]
    allDirections = [(u, v) | u <- [-1, 0, 1], v <- [-1, 0, 1], u /= 0 || v /= 0]

    n = length input
    m = length (input !! 0)

solve2 :: Input -> Int
solve2 input = length $ filter xmas allPositions
  where
    xmas (i, j)
      | isBetween 1 (n - 1) i && isBetween 1 (m - 1) j =
          any
            (all isCharAt)
            [ [(i, j, 'A'), (i - 1, j - 1, 'M'), (i + 1, j + 1, 'S'), (i - 1, j + 1, 'M'), (i + 1, j - 1, 'S')],
              [(i, j, 'A'), (i - 1, j - 1, 'M'), (i + 1, j + 1, 'S'), (i - 1, j + 1, 'S'), (i + 1, j - 1, 'M')],
              [(i, j, 'A'), (i - 1, j - 1, 'S'), (i + 1, j + 1, 'M'), (i - 1, j + 1, 'M'), (i + 1, j - 1, 'S')],
              [(i, j, 'A'), (i - 1, j - 1, 'S'), (i + 1, j + 1, 'M'), (i - 1, j + 1, 'S'), (i + 1, j - 1, 'M')]
            ]
      | otherwise = False

    isBetween a b x = x >= a && x < b

    isCharAt (i, j, c) = input !! i !! j == c

    allPositions = [(i, j) | i <- [0 .. n - 1], j <- [0 .. m - 1]]

    n = length input
    m = length (input !! 0)
