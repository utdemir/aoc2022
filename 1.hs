{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, TypeApplications #-}

import Relude
import Data.Attoparsec.ByteString.Char8 (many1, decimal, Parser)
import Data.Foldable (maximum)
import Util

parser :: Parser [[Int]]
parser = many1 (single <* "\n")
    where
        single :: Parser [Int]
        single = many1 (decimal <* "\n")

solve1 :: [[Int]] -> Int
solve1 xs =
  xs 
    & map sum
    & maximum

solve2 :: [[Int]] -> Int
solve2 xs =
  xs 
    & map sum
    & sortOn Down
    & take 3
    & sum


main :: IO ()
main = aoc "1.txt" parser solve1 solve2