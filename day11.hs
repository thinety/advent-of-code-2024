import Control.Arrow
import Control.Monad.State
import Data.List
import Data.Map qualified as Map

main :: IO ()
main = interact $ parse >>> solve1 &&& solve2 >>> \(ans1, ans2) -> (show ans1) ++ "\n" ++ (show ans2) ++ "\n"

type Input = [Int]

parse :: String -> Input
parse = lines >>> head >>> words >>> map read

solve1 :: Input -> Int
solve1 = map (\x -> evalState (stones 25 x) Map.empty) >>> sum

solve2 :: Input -> Int
solve2 = map (\x -> evalState (stones 75 x) Map.empty) >>> sum

stones :: Int -> Int -> State (Map.Map (Int, Int) Int) Int
stones n x = do
  memo <- get
  case Map.lookup (n, x) memo of
    Just ans -> pure ans
    Nothing -> do
      ans <- stones' n x
      put $ Map.insert (n, x) ans memo
      pure ans

stones' :: Int -> Int -> State (Map.Map (Int, Int) Int) Int
stones' n x
  | n == 0 = pure 1
  | x == 0 = stones (n - 1) 1
  | odd len = stones (n - 1) (x * 2024)
  | otherwise = do
      a <- stones (n - 1) left
      b <- stones (n - 1) right
      pure $ a + b
  where
    xStr = show x
    len = length xStr
    len2 = len `div` 2
    left = read $ take len2 xStr
    right = read $ drop len2 xStr
