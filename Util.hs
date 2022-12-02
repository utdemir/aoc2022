{-# LANGUAGE NoImplicitPrelude #-}

module Util (aoc) where

import Relude
import Data.Attoparsec.ByteString

aoc 
  :: (Show b, Show c)
  => FilePath 
  -> Parser a
  -> (a -> b)
  -> (a -> c)
  -> IO ()
aoc path parser step1 step2 = do
    contents <- readFileBS path
    case parseOnly parser contents of
        Left err -> error (toText err)
        Right cs -> do
            let solution1 = step1 cs
            let solution2 = step2 cs
            print solution1
            print solution2