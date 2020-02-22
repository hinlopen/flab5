module Player where

import Graphics.Gloss
import KinematicBody

data Player = Player {position :: Point,
                      speed :: Float,
                      size:: Float,
                      lives :: Int,
                      score :: Float,
                      animation_index :: Int,
                      playing_animation :: Bool}

instance KinematicBody Player where
    move p v = p { position = (position p) + v}
    move_to p pos = p { position = pos}
