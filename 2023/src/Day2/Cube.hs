{-# LANGUAGE OverloadedStrings #-}

module Day2.Cube (solveCube) where

import qualified Text.Megaparsec as M
import Text.Megaparsec.Char.Lexer as L
import Data.Foldable (Foldable(foldl'))
import Text.Megaparsec.Byte (space1, string, string')
import Data.Void (Void)
import Text.Megaparsec (some)
import Data.Text (Text, pack, unpack)
import qualified Text.Megaparsec.Char as M
import Day2.GameParser

solveCube :: IO ()
solveCube = do
  let maxes = Round {blue=14, green=13, red=12}
  games <- parse "src/Day2/day2.test"
  print $ "Day 2 Part 1: " ++ show (sumPossibleGames maxes games)

sumPossibleGames :: Round -> [Game] -> Int
sumPossibleGames maxes games = sum . map gameId $ filterPossibleGames maxes games

filterPossibleGames :: Round -> [Game] -> [Game]
filterPossibleGames maxes games = filter (gamePossible maxes) games

gamePossible :: Round -> Game -> Bool
gamePossible maxes game = 
  all (roundImpossible maxes) $ rounds game 

roundImpossible :: Round -> Round -> Bool
roundImpossible maxes rd =
  blue rd <= blue maxes && green rd <= green maxes && red rd <= red maxes

drawPossible :: (Int, Int) -> Bool
drawPossible (round, max) = round <= max


