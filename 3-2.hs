{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, TypeApplications, LambdaCase #-}

import Relude
import Data.Attoparsec.ByteString.Char8 (many1, decimal, Parser, string, notChar, endOfLine, sepBy)
import Data.Foldable (maximum)
import Util
import qualified Data.Set as Set
import Distribution.Simple.Utils (xargs)
import Distribution.Compat.Lens (_1)
import Data.List (foldl1')

parser :: Parser [[String]]
parser = many1 (replicateM 3 line)
    where
        line :: Parser String
        line = many1 (notChar '\n') <* "\n"

solve1 :: [[String]] -> Int
solve1 xs = 
    xs
        & map (map Set.fromList)
        & map (foldl1' Set.intersection)
        & map (\i -> Set.toList i & map priority & sum)
        & sum

priority :: Char -> Int
priority c 
    | c >= 'a' && c <= 'z' = ord c - ord 'a' + 1
    | c >= 'A' && c <= 'Z' = ord c - ord 'A' + 27
    | otherwise = error "bad char"

solve2 :: a -> Int
solve2 xs = 0

main :: IO ()
main = aoc "3.txt" parser solve1 solve2