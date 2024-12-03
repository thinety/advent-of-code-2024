import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.List
import Data.Maybe
import Text.Read

main :: IO ()
main = interact $ parse >>> solve1 &&& solve2 >>> \(ans1, ans2) -> (show ans1) ++ "\n" ++ (show ans2) ++ "\n"

type Input = String

parse :: String -> Input
parse = id

solve1 :: Input -> Int
solve1 = tails >>> map mulInstruction >>> catMaybes >>> sum

solve2 :: Input -> Int
solve2 = go True
  where
    go on "" = 0
    go on s@(_ : s') =
      let val = fromMaybe 0 $ guard on >> mulInstruction s
          on' = fromMaybe on $ doInstruction s
       in val + go on' s'

mulInstruction :: String -> Maybe Int
mulInstruction s0 = do
  guard (isPrefixOf "mul(" s0)
  let s1 = drop 4 s0
      (x, s2) = break (== ',') s1
  guard (not $ null s2)
  let s3 = drop 1 s2
      (y, s4) = break (== ')') s3
  guard (not $ null s3)
  x' <- readMaybe x
  y' <- readMaybe y
  pure (x' * y')

doInstruction :: String -> Maybe Bool
doInstruction s = doIns <|> dontIns
  where
    doIns = guard (isPrefixOf "do()" s) >> pure True
    dontIns = guard (isPrefixOf "don't()" s) >> pure False
