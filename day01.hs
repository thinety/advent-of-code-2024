import Control.Arrow
import Data.List
import Data.Map qualified as Map
import Data.Maybe

main :: IO ()
main = interact $ parse >>> solve1 &&& solve2 >>> \(ans1, ans2) -> (show ans1) ++ "\n" ++ (show ans2) ++ "\n"

type Input = ([Int], [Int])

parse :: String -> Input
parse = lines >>> map (words >>> map read >>> (\[a, b] -> (a, b))) >>> unzip

solve1 :: Input -> Int
solve1 = sort *** sort >>> uncurry (zipWith (-)) >>> map abs >>> sum

solve2 :: Input -> Int
solve2 =
  second countMap >>> (\(xs, m) -> map (similarityScore m) xs) >>> sum
  where
    countMap = sort >>> group >>> map head &&& map length >>> uncurry (zip) >>> Map.fromList

    similarityScore m k = k * occurrences m k

    occurrences m k = fromMaybe 0 $ Map.lookup k m
