import Control.Arrow

main :: IO ()
main = interact $ parse >>> solve1 &&& solve2 >>> \(ans1, ans2) -> (show ans1) ++ "\n" ++ (show ans2) ++ "\n"

type Input = [[Int]]

parse :: String -> Input
parse = lines >>> map (words >>> map read)

solve1 :: Input -> Int
solve1 = filter safe >>> length

solve2 :: Input -> Int
solve2 = filter (modifiedReports >>> any safe) >>> length

safe :: [Int] -> Bool
safe = differences >>> allBetween 1 3 &&& allBetween (-3) (-1) >>> uncurry (||)
  where
    differences = (drop 1) &&& id >>> uncurry (zipWith (-))

    allBetween a b = all ((>= a) &&& (<= b) >>> uncurry (&&))

modifiedReports :: [Int] -> [[Int]]
modifiedReports xs = go 0
  where
    n = length xs

    go i
      | i < n = removeNth i xs : go (i + 1)
      | otherwise = []

    removeNth n = splitAt n >>> second tail >>> uncurry (++)
