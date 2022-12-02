{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, TypeApplications, LambdaCase #-}

import Relude
import Data.Attoparsec.ByteString.Char8 (many1, decimal, Parser, string)
import Data.Foldable (maximum)
import Util

data Piece = Rock | Paper | Scissors
  deriving (Eq)
data Winner = WLeft | WRight | WDraw
data RHS = X | Y | Z

parser :: Parser [(Piece, RHS)]
parser = many1 ((,) <$> pieceL <* " " <*> pieceR <* "\n")
    where
        pieceL :: Parser Piece
        pieceL = string "A" $> Rock
            <|> string "B" $> Paper
            <|> string "C" $> Scissors
        pieceR :: Parser RHS
        pieceR = string "X" $> X
            <|> string "Y" $> Y
            <|> string "Z" $> Z

pieceScore :: Piece -> Int
pieceScore Rock = 1
pieceScore Paper = 2
pieceScore Scissors = 3

roundScore :: Piece -> Piece -> Int
roundScore m1 m2 = winner m1 m2 & \case
    WLeft -> 0
    WDraw -> 3
    WRight -> 6

winner :: Piece -> Piece -> Winner
winner Rock Paper = WRight
winner Paper Scissors = WRight
winner Scissors Rock = WRight
winner a b | a == b = WDraw
winner _ _ = WLeft

solve1 :: [(Piece, RHS)] -> Int
solve1 xs = 
    xs 
        & map (\(p1, rhs) -> (
            p1, 
            case rhs of
                X -> Rock
                Y -> Paper
                Z -> Scissors
            ))
        & map (\(opponent, self) -> pieceScore self + roundScore opponent self)
        & sum

solve2 :: [(Piece, RHS)] -> Int
solve2 xs = 
    xs 
        & map (\(p1, rhs) -> (
            p1, 
            case rhs of
                X -> WLeft
                Y -> WDraw
                Z -> WRight
            ))
        & map (\(opponent, self) -> 
            let chosenPiece = case (opponent, self) of
                    (x, WDraw) -> x
                    (Rock, WLeft) -> Scissors
                    (Rock, WRight) -> Paper
                    (Paper, WLeft) -> Rock
                    (Paper, WRight) -> Scissors
                    (Scissors, WLeft) -> Paper
                    (Scissors, WRight) -> Rock
            in pieceScore chosenPiece + roundScore opponent chosenPiece
        )
        & sum

main :: IO ()
main = aoc "2.txt" parser solve1 solve2