import Control.Applicative
import Control.Arrow
import Data.List
import Data.Map qualified as Map
import Data.Maybe
import Data.Text qualified as T

main :: IO ()
main = interact $ parse >>> solve1 &&& solve2 >>> \(ans1, ans2) -> (show ans1) ++ "\n" ++ (show ans2) ++ "\n"

type Input = ([(Int, Int)], [[Int]])

parse :: String -> Input
parse = T.pack >>> T.splitOn doubleLineBreak >>> tuplify2 >>> rules *** updates
  where
    rules = T.lines >>> map (T.splitOn pipe >>> map (T.unpack >>> read) >>> tuplify2)
    updates = T.lines >>> map (T.splitOn comma >>> map (T.unpack >>> read))

    tuplify2 [x, y] = (x, y)

    doubleLineBreak = T.pack "\n\n"
    pipe = T.pack "|"
    comma = T.pack ","

solve1 :: Input -> Int
solve1 = uncurry solve1'
  where
    solve1' rules = filter (updateOk rules) >>> map middle >>> sum

solve2 :: Input -> Int
solve2 = uncurry solve2'
  where
    solve2' rules = filter (updateOk rules >>> not) >>> map (sortUpdate rules >>> middle) >>> sum

updateOk :: [(Int, Int)] -> [Int] -> Bool
updateOk rules update = all ruleOk rules
  where
    ruleOk rule = fromMaybe True $ do
      let (a, b) = rule
      ia <- Map.lookup a indexes
      ib <- Map.lookup b indexes
      pure $ ia < ib

    indexes = Map.fromList . flip zip [0 ..] $ update

sortUpdate :: [(Int, Int)] -> [Int] -> [Int]
sortUpdate rules = sortBy compare
  where
    compare a b =
      let lt = Map.lookup (a, b) relations >> pure LT
          gt = Map.lookup (b, a) relations >> pure GT
       in fromJust $ lt <|> gt

    relations = Map.fromList $ zip rules (repeat ())

middle :: [a] -> a
middle update = update !! (length update `div` 2)
