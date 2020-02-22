module Constants where

import Graphics.Gloss
import Helpers

worldWidth :: Int
worldWidth = 1024

worldHeight :: Int
worldHeight = 680

worldWidthHalf :: Float
worldWidthHalf = (fromIntegral worldWidth)/2

worldHeightHalf :: Float
worldHeightHalf = (fromIntegral worldHeight) /2

worldColor :: Color
worldColor = fromRGB 236 208 120 256

player_size :: Float
player_size = 40

starting_lives :: Int
starting_lives = 3

player_starting_color :: Color
player_starting_color = fromRGB 114 123 140 256