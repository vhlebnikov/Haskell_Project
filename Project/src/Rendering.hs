module Rendering where

import Data.Array

import Graphics.Gloss

import Game

boardGridColor = white
playerXColor = blue
playerOColor = red
tieColor = greyN 0.5

xCell :: Picture
xCell = pictures [rotate 45.0 $ rectangleSolid 100 12.0, rotate (-45.0) $ rectangleSolid 100 12.0]

oCell :: Picture
oCell = thickCircle 40 10.0

outcomeColor :: Cell -> Color
outcomeColor (Just PlayerX) = playerXColor
outcomeColor (Just PlayerO) = playerOColor
outcomeColor Nothing = tieColor

snapPictureToCell :: Picture -> (Int, Int) -> Picture
snapPictureToCell picture (row, column) = translate x y picture
    where x = cellWidth * (fromIntegral column + 0.5)
          y = cellHeight * (fromIntegral row + 0.5)

cellsOfBoard :: Board -> Cell -> Picture -> Picture
cellsOfBoard board cell cellPicture = pictures $ map (snapPictureToCell cellPicture . fst) $ filter (\((_, _), e) -> e == cell) $ assocs board

xCellsOfBoard :: Board -> Picture
xCellsOfBoard board = cellsOfBoard board (Just PlayerX) xCell

oCellsOfBoard :: Board -> Picture
oCellsOfBoard board = cellsOfBoard board (Just PlayerO) oCell

boardGrid :: Picture
boardGrid = pictures $ concatMap (\x -> [line [(x * cellWidth, 0), (x * cellWidth, cellHeight * 3)], line [(0, x * cellHeight), (cellWidth * 3, x * cellHeight)]]) [0..3]

boardAsRunningPicture :: Board -> Picture
boardAsRunningPicture board = pictures [color playerXColor $ xCellsOfBoard board, color playerOColor $ oCellsOfBoard board, color boardGridColor boardGrid]

boardAsGameOverPicture :: Cell -> Board -> Picture
boardAsGameOverPicture winner board = color (outcomeColor winner) $ pictures [xCellsOfBoard board, oCellsOfBoard board, boardGrid]

gameAsPicture :: Game -> Picture
gameAsPicture game = translate (fromIntegral screenWidth * (-0.5)) (fromIntegral screenHeight * (-0.5)) frame
    where frame | gameState game == Running = boardAsRunningPicture (gameBoard game)
                | gameState game == GameOver (Just PlayerX) = boardAsGameOverPicture (Just PlayerX) (gameBoard game)
                | gameState game == GameOver (Just PlayerO) = boardAsGameOverPicture (Just PlayerO) (gameBoard game)
                | gameState game == GameOver Nothing = boardAsGameOverPicture Nothing (gameBoard game)