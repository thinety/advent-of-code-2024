module Main where

import Base
import Data.ByteString.Lazy.Char8 qualified as C
import Data.List
import Data.Map qualified as Map
import Data.Maybe

main :: IO ()
main = C.interact <| parse .> solve .> C.pack

type Input = ([Int], [Int])

parse :: C.ByteString -> Input
parse =
  runScanner <| do
    l <-
      many <| do
        a <- int
        b <- int
        pure (a, b)
    pure (map fst l, map snd l)

solve :: Input -> String
solve input =
  let ans1 = solve1 input
      ans2 = solve2 input
   in (show ans1) ++ "\n" ++ (show ans2) ++ "\n"

solve1 :: Input -> Int
solve1 (xs, ys) =
  let sortedXs = sort xs
      sortedYs = sort ys
   in zipWith (-) sortedXs sortedYs |> map abs |> sum

solve2 :: Input -> Int
solve2 (xs, ys) =
  let groups = ys |> sort |> group
      keys = groups |> map head
      values = groups |> map length
      count = zip keys values |> Map.fromList
      occurrences = xs |> map (count Map.!?) |> map (fromMaybe 0)
   in zipWith (*) xs occurrences |> sum
