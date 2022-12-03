{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, TypeApplications, LambdaCase #-}

import Relude
import Data.Attoparsec.ByteString.Char8 (many1, decimal, Parser, string, notChar)
import Data.Foldable (maximum)
import Util
import qualified Data.Set as Set
import Distribution.Simple.Utils (xargs)

parser :: Parser [(String, String)]
parser = many1 (single <* "\n")
    where
        single :: Parser (String, String)
        single = do
            xs <- many1 (notChar '\n')
            return $ splitAt (length xs `div` 2) xs

solve1 :: [(String, String)] -> Int
solve1 xs = 
    xs
        & map (\(l, r) -> (Set.fromList l, Set.fromList r))
        & map (\(l, r) -> Set.intersection l r)
        & map (\i -> Set.toList i & map priority & sum)
        & sum

priority :: Char -> Int
priority c 
    | c >= 'a' && c <= 'z' = ord c - ord 'a' + 1
    | c >= 'A' && c <= 'Z' = ord c - ord 'A' + 27
    | otherwise = error "bad char"

solve2 :: [(String, String)] -> Int
solve2 xs = 0

main :: IO ()
main = aoc "3.txt" parser solve1 solve2