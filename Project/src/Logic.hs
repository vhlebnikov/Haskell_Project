module Logic where

import Data.Array
import Data.Foldable ( asum )

import Game
import Graphics.Gloss.Interface.Pure.Game

switchPlayer :: Game -> Game
switchPlayer game | gamePlayer game == PlayerX = game {gamePlayer = PlayerO}
                  | gamePlayer game == PlayerO  = game {gamePlayer = PlayerX}

playerWon :: Player -> Board -> Bool 
playerWon player board = any check combinations
    where combinations = rows ++ column ++ diag
          rows = [[(i, j) | j <- [0..2]] | i <- [0..2]]
          column = [[(i, j) | i <- [0..2]] | j <- [0..2]]
          diag = [[(i,i) | i <- [0..2]], [(2 - i, i) | i <- [0..2]]]
          check comb = (3 ==) $ length $ filter (\x -> x == (Just player)) $ map (\x -> board ! x) comb
countCells :: Cell -> Board -> Int
countCells cell board = length $ filter (cell ==) $ elems board

checkGameOver :: Game -> Game
checkGameOver game | playerWon PlayerX (gameBoard game) = game {gameState = GameOver (Just PlayerX)}
                   | playerWon PlayerO (gameBoard game) = game {gameState = GameOver (Just PlayerO)}
                   | countCells Nothing (gameBoard game) == 0 = game {gameState = GameOver Nothing}
                   | otherwise = game

playerTurn :: Game -> (Int, Int) -> Game
playerTurn game cellCoord
    | inRange ((0, 0), (2, 2)) cellCoord && (gameBoard game ! cellCoord == Nothing) = 
        checkGameOver $ switchPlayer $ game {gameBoard = gameBoard game // [(cellCoord, Just $ gamePlayer game)]}
    | otherwise = game

-- смещение из-за того начало координат находится в центре
mousePosAsCellCoord :: (Float, Float) -> (Int, Int)
mousePosAsCellCoord (x, y) = (floor ((y + fromIntegral screenHeight * 0.5) / cellHeight), floor ((x + fromIntegral screenWidth * 0.5) / cellWidth))

transformGame :: Event -> Game -> Game
transformGame (EventKey (MouseButton LeftButton) Up _ mousePos) game
    | gameState game == Running = playerTurn game $ mousePosAsCellCoord mousePos
    | otherwise = initialGame
transformGame _ game = game