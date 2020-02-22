module Input where

import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss
import Model
import Controller
import KinematicBody
import Player

input :: Event -> World -> IO World
input e world = return (inputKey e world)

inputKey :: Event -> World -> World
inputKey (EventKey (Char 'p') Down _ _) world
  --Handle p key first for (un)pausing
    | paused world = world { game_state = Playing } 
    | otherwise    = world { game_state = Paused } 

inputKey (EventKey (Char key) Down _ _) world 
    | paused world = world 
    | otherwise = let p = player world 
                      s = Player.speed p
                  in case key of
                    'w' -> world { player = move p ( 0,  s) }
                    's' -> world { player = move p ( 0, -s) }
                    'd' -> world { player = move p ( s,  0) }
                    'a' -> world { player = move p (-s,  0) }
                    'r' -> initialWorld
                    _   -> world
                    
inputKey (EventKey (SpecialKey key) Down _ _) world
  | paused world = world
  | otherwise = case key of
                    KeySpace -> blast world                                                
                    _ -> world

inputKey (EventKey (MouseButton LeftButton) Down _ mouse_pos) world 
    | paused world = world
    | otherwise = shoot mouse_pos world

inputKey _ world = world -- Don't respond to non-key events
