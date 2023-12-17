{-# LANGUAGE OverloadedStrings #-}

module Day2.GameParser (Game(..), Round(..), parse) where

import qualified Text.Megaparsec as M
import Text.Megaparsec.Byte (space1, string, string')
import Data.Void (Void)
import Data.Text (Text, pack, unpack)
import Text.Megaparsec (some)
import qualified Text.Megaparsec.Char as M

type Parser = M.Parsec Void Text

data Game = Game { gameId :: Int
                 , rounds :: [Round]
                 } deriving (Show)

data Round = Round { blue :: Int, green :: Int, red :: Int } deriving (Show)

parse :: FilePath -> IO [Game]
parse filePath = do
  contents <- readFile filePath
  case M.runParser gameListParser "" (pack contents) of
    Left s -> error (show s)
    Right games -> return games

gameListParser :: Parser [Game]
gameListParser = do pg `M.sepEndBy` M.eol

pg :: Parser Game
pg = do
  M.string' "Game "
  gameId <- some M.digitChar
  M.string' ": "
  rds <- roundParser `M.sepBy` M.string "; "
  return $ Game {gameId=read gameId, rounds=rds}

roundParser :: Parser Round
roundParser = do
  round <- parseDraw `M.sepBy` M.string ", "
  return $ foldr roundCons Round {blue=0, green=0, red=0} round

roundCons :: (Int, String) -> Round -> Round
roundCons (amount, color) (Round blue green red)
  | color == "blue" = Round (blue + amount) green red
  | color == "green" = Round blue (green + amount) red
  | color == "red" = Round blue green (red + amount)
  | otherwise = Round blue green red

parseDraw :: Parser (Int, String)
parseDraw = do
  cnt <- some M.digitChar
  M.space
  color <- some M.alphaNumChar
  return (read cnt, color)

