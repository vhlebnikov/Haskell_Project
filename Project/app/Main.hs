module Main where

import Graphics.Gloss

import Game
import Logic
import Rendering

main :: IO ()
main = play window backgroundColor 30 initialGame gameAsPicture transformGame (const id)
       where window = InWindow "Game" (screenWidth, screenHeight) (400, 200)
             backgroundColor = black
