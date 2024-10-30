module Base where

import Control.Monad
import Control.Monad.State
import Data.ByteString.Lazy.Char8 qualified as C
import Data.Char
import Data.Maybe

infixl 0 |>

(|>) :: a -> (a -> b) -> b
(|>) = flip ($)

infixr 0 <|

(<|) :: (a -> b) -> a -> b
(<|) = ($)

infixl 9 .>

(.>) :: (a -> b) -> (b -> c) -> (a -> c)
(.>) = flip (.)

infixr 9 <.

(<.) :: (b -> c) -> (a -> b) -> (a -> c)
(<.) = (.)

type Scanner = State C.ByteString

scanner :: (C.ByteString -> (a, C.ByteString)) -> Scanner a
scanner = state

runScanner :: Scanner a -> C.ByteString -> a
runScanner = evalState

line :: Scanner C.ByteString
line =
  scanner <| \s ->
    let (l, s') = C.break (== '\n') s in (l, C.drop 1 s')

word :: Scanner C.ByteString
word = scanner <| C.dropWhile isSpace .> C.break isSpace

int :: Scanner Int
int = scanner <| C.dropWhile isSpace .> C.readInt .> fromJust

list :: Int -> Scanner a -> Scanner [a]
list = replicateM

eof :: Scanner Bool
eof = scanner <| C.dropWhile isSpace .> \s -> (C.null s, s)

many :: Scanner a -> Scanner [a]
many s = eof >>= \e -> if e then pure [] else liftA2 (:) s (many s)
