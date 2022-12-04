{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, TypeApplications, LambdaCase #-}

import Relude
import Data.Attoparsec.ByteString.Char8 (many1, decimal, Parser)
import Util

data Range = Range Int Int

type ParserOutput = [(Range, Range)]
parser :: Parser ParserOutput
parser = many1 (line <* "\n")
    where
        line = (,) <$> range <* "," <*> range
        range = Range <$> decimal <* "-" <*> decimal

fullyOverlaps :: Range -> Range -> Bool
fullyOverlaps (Range l1 r1) (Range l2 r2) = l1 <= l2 && r1 >= r2

solve1 :: ParserOutput -> Int
solve1 xs = 
    xs 
        & filter (\(l, r) -> l `fullyOverlaps` r || r `fullyOverlaps` l)
        & length

overlaps :: Range -> Range -> Bool
overlaps (Range l1 r1) (Range l2 r2) = l1 <= l2 && r1 >= l2 || l2 <= l1 && r2 >= l1

solve2 :: ParserOutput -> Int
solve2 xs = 
    xs 
        & filter (\(l, r) -> l `overlaps` r || r `overlaps` l)
        & length

main :: IO ()
main = aoc "4.txt" parser solve1 solve2