import Control.Arrow
import Data.List
import Data.Maybe

main :: IO ()
main = interact $ parse >>> solve1 &&& solve2 >>> \(ans1, ans2) -> (show ans1) ++ "\n" ++ (show ans2) ++ "\n"

type Input = [(Int, [Int])]

parse :: String -> Input
parse = lines >>> map (words >>> uncons >>> fromJust >>> first init >>> read *** map read)

solve1 :: Input -> Int
solve1 = filter (uncurry ok) >>> map fst >>> sum
  where
    ok x = reverse >>> vals >>> any (== x)

    vals [y] = [y]
    vals (y : ys) = do
      x <- vals ys
      [y + x, y * x]

solve2 :: Input -> Int
solve2 = filter (uncurry ok) >>> map fst >>> sum
  where
    ok x = reverse >>> vals >>> any (== x)

    vals [y] = [y]
    vals (y : ys) = do
      x <- vals ys
      [x + y, x * y, read $ (show x) ++ (show y)]
