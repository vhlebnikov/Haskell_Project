module Game where

import Data.Array

data Player = PlayerX | PlayerO deriving (Eq, Show)
type Cell = Maybe Player
data State = Running | GameOver (Maybe Player) deriving (Eq, Show)

type Board = Array (Int, Int) Cell

data Game = Game { gameBoard :: Board
                 , gamePlayer :: Player
                 , gameState :: State
                 } deriving (Eq, Show)

screenWidth :: Int
screenWidth = 640

screenHeight :: Int
screenHeight = 480

cellWidth :: Float
cellWidth = fromIntegral screenWidth / 3

cellHeight :: Float
cellHeight = fromIntegral screenHeight / 3

initialGame :: Game
initialGame = Game { gameBoard = array ((0, 0), (2, 2)) $ zip (range ((0, 0), (2, 2))) (repeat Nothing)
                   , gamePlayer = PlayerX
                   , gameState = Running
                   }