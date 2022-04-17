module Main where

import Data.Array
import Data.Maybe
import Graphics.Gloss

data Player = PlayerX | PlayerO deriving (Eq, Show)
type Cell = Maybe Player
data State = Running | GameOver (Maybe Player) deriving (Eq, Show)

type Board = Array (Int, Int) Cell

data Game = Game { gameBoard :: Board
                 , gamePlayer :: Player
                 , gameState :: State
                 } deriving (Eq, Show)

main :: IO ()
main = play window backgroundcolor 30 initgame gamepicture stepofgame (const id)
       where window = InWindow "Game" (screenWidth, screenHeight) (400, 200)
             backgroundcolor = black

screenWidth :: Int
screenWidth = 640

screenHeight :: Int
screenHeight = 480

cellWidth :: Float
cellWidth = fromIntegral screenWidth / 3

cellHeight :: Float
cellHeight = fromIntegral screenHeight / 3

initgame :: Game
initgame = Game { gameBoard = array ((0, 0), (2, 2)) $ zip (range ((0, 0), (2, 2))) (repeat Nothing)
                   , gamePlayer = PlayerX
                   , gameState = Running
                   }

boardcolor = white
xcolor = blue
ocolor = red
tiecolor = greyN 0.5

xcell :: Picture
xCell = pictures [rotate 45.0 $ rectangleSolid 100 12.0, rotate (-45.0) $ rectangleSolid 100 12.0]

ocell :: Picture
oCell = thickCircle 40 10.0

playercolor :: Cell -> Color
playercolor (Just PlayerX) = xcolor
playercolor (Just PlayerO) = ocolor
playercolor Nothing = tiecolor

picturecoord :: Picture -> (Int, Int) -> Picture
picturecoord picture (row, column) = translate x y picture
    where x = cellWidth * (fromIntegral column + 0.5)
          y = cellHeight * (fromIntegral row + 0.5)

allcells :: Board -> Cell -> Picture -> Picture
allcells board cell cellpicture = pictures $ map (picturecoord cellpicture . fst) $ filter (\((_, _), e) -> e == cell) $ assocs board

xcells :: Board -> Picture
xcells board = allcells board (Just PlayerX) xCell

ocells :: Board -> Picture
ocells board = allcells board (Just PlayerO) oCell

drawboardgrid :: Picture
drawboardgrid = pictures $ concatMap (\x -> [line [(x * cellWidth, 0), (x * cellWidth, cellHeight * 3)], line [(0, x * cellHeight), (cellWidth * 3, x * cellHeight)]]) [0..3]

runningboard :: Board -> Picture
runningboard board = pictures [color xcolor $ xcells board, color ocolor $ ocells board, color boardcolor drawboardgrid]

gameoverboard :: Cell -> Board -> Picture
gameoverboard winner board = color (playercolor winner) $ pictures [xcells board, ocells board, drawboardgrid]

gamepicture :: Game -> Picture
gamepicture game = translate (fromIntegral screenWidth * (-0.5)) (fromIntegral screenHeight * (-0.5)) frame
    where frame | gameState game == Running = runningboard (gameBoard game)
                | gameState game == GameOver (Just PlayerX) = gameoverboard (Just PlayerX) (gameBoard game)
                | gameState game == GameOver (Just PlayerO) = gameoverboard (Just PlayerO) (gameBoard game)
                | gameState game == GameOver Nothing = gameoverboard Nothing (gameBoard game)

switchplayer :: Game -> Game
switchplayer game | gamePlayer game == PlayerX = game {gamePlayer = PlayerO}
                  | gamePlayer game == PlayerO  = game {gamePlayer = PlayerX}

playerwin :: Player -> Board -> Bool
playerwin player board = any (3 ==) $ length $ filter (\x -> x == Just player) $ map (board !) combinations
    where combinations :: [(Int, Int)]
          combinations = rows ++ columns ++ diag
          rows = [[(i, j) | j <- [0..2]] | i <- [0..2]]
          columns = [[(i, j) | i <- [0..2]] | j <- [0..2]]
          diag = [[(i,i) | i <- [0..2]], [(2 - i, i) | i <- [0..2]]]
          check comb = (3 ==) $ length $ filter (\x -> x == Just player) $ map (board !) comb

countcells :: Cell -> Board -> Int
countcells cell board = length $ filter (cell ==) $ elems board

gameoverflag :: Game -> Game
gameoverflag game | playerwin PlayerX (gameBoard game) = game {gameState = GameOver (Just PlayerX)}
                   | playerwin PlayerO (gameBoard game) = game {gameState = GameOver (Just PlayerO)}
                   | countcells Nothing (gameBoard game) == 0 = game {gameState = GameOver Nothing}
                   | otherwise = game

playerTurn :: Game -> (Int, Int) -> Game
playerTurn game cellcoord
    | inRange ((0, 0), (2, 2)) cellcoord && isNothing (gameBoard game ! cellcoord) =
        gameoverflag $ switchplayer $ game {gameBoard = gameBoard game // [(cellcoord, Just $ gamePlayer game)]}
    | otherwise = game

-- смещение из-за того начало координат находится в центре
mousetocell :: (Float, Float) -> (Int, Int)
mousetocell (x, y) = (floor ((y + fromIntegral screenHeight * 0.5) / cellHeight), floor ((x + fromIntegral screenWidth * 0.5) / cellWidth))

stepofgame :: Event -> Game -> Game
stepofgame (EventKey (MouseButton LeftButton) Up _ mousePos) game
    | gameState game == Running = playerTurn game $ mousetocell mousePos
    | otherwise = initialGame
stepofgame _ game = game