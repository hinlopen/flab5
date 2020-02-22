module Main where

import Graphics.Gloss.Interface.IO.Game
import Controller
import Constants
import Model
import View
import Input

main :: IO ()
main = do 
          putStrLn "WASD to walk around"
          putStrLn "Click to fire"
          putStrLn "Press [Space] to have a blast"
          contents <- readFile "userdata.txt"
          putStrLn contents
          playIO (InWindow contents (worldWidth, worldHeight) (0, 0))
              worldColor
              fps 
              initialWorld
              view
              input
              step

fps :: Int
fps = 60