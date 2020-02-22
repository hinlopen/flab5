module Projectile where

import Graphics.Gloss
import KinematicBody

data Projectile = Projectile {position :: Point, direction :: Vector, speed :: Float, size :: Float}

instance KinematicBody Projectile where
    move p v = p { position = (position p) + v }
    move_to p pos = p { position = pos }