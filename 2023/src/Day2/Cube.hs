{-# LANGUAGE OverloadedStrings #-}

module Day2.Cube (solveDay2) where

import qualified Text.Megaparsec as M
import Text.Megaparsec.Char.Lexer as L
import Data.Foldable (Foldable(foldl'))
import Text.Megaparsec.Byte (space1, string, string')
import Data.Void (Void)
import Text.Megaparsec (some)
import Data.Text (Text, pack, unpack)
import qualified Text.Megaparsec.Char as M
import Day2.GameParser

solveDay2 :: IO ()
solveDay2 = do
  let maxes = Round {blue=14, green=13, red=12}
  games <- parse "src/Day2/day2.txt"
  print $ "Day 2 Part 1: " ++ show (sumPossibleGames maxes games)
  print $ "Day 2 Part 2: " ++ show (sum . map roundPower $ minPossible games)

-- Part 1
sumPossibleGames :: Round -> [Game] -> Int
sumPossibleGames maxes games = sum . map gameId $ filterPossibleGames maxes games

filterPossibleGames :: Round -> [Game] -> [Game]
filterPossibleGames maxes = filter (gamePossible maxes)

gamePossible :: Round -> Game -> Bool
gamePossible maxes game = 
  all (roundImpossible maxes) $ rounds game 

roundImpossible :: Round -> Round -> Bool
roundImpossible maxes rd =
  blue rd <= blue maxes && green rd <= green maxes && red rd <= red maxes

drawPossible :: (Int, Int) -> Bool
drawPossible (round, max) = round <= max

-- Part 2
minPossible :: [Game] -> [Round]
minPossible = map minPossibleRound

minPossibleRound :: Game -> Round
minPossibleRound game = foldr foldRound Round {blue=0, green=0, red=0} $ rounds game

foldRound :: Round -> Round -> Round
foldRound (Round blue green red) (Round minB minG minR) =
  Round (max blue minB) (max green minG) (max red minR)

roundPower :: Round -> Int

roundPower (Round blue green red) = blue * green * red
