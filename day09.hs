import Control.Arrow
import Data.List
import Data.Map qualified as Map

main :: IO ()
main = interact $ parse >>> solve1 &&& solve2 >>> \(ans1, ans2) -> (show ans1) ++ "\n" ++ (show ans2) ++ "\n"

type Input = [Int]

parse :: String -> Input
parse = lines >>> head >>> map (singleton >>> read)

solve1 :: Input -> Int
solve1 diskMap = checksum
  where
    checksum = go 0 blocks (reverse blocks)
      where
        go :: Int -> [Int] -> [Int] -> Int
        go i ~(x : xs) ~(y : ys)
          | i < n =
              if x >= 0
                then
                  i * x + go (i + 1) xs (y : ys)
                else
                  if y >= 0
                    then
                      i * y + go (i + 1) xs ys
                    else go i (x : xs) ys
          | otherwise = 0

    blocks = go True 0 diskMap
      where
        go :: Bool -> Int -> [Int] -> [Int]
        go _ _ [] = []
        go isFile fileId (x : xs) =
          let current = if isFile then replicate x fileId else replicate x (-1)
              isFile' = not isFile
              fileId' = if isFile then fileId + 1 else fileId
           in current ++ go isFile' fileId' xs

    n = zip ([0 ..]) >>> filter (fst >>> even) >>> map snd >>> sum $ diskMap

solve2 :: Input -> Int
solve2 = solve1

checksum :: [Int] -> Int
checksum = zip [0 ..] >>> filter (snd >>> (>= 0)) >>> map (uncurry (*)) >>> sum
