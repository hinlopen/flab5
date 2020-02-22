module KinematicBody where

import Graphics.Gloss

class KinematicBody a where
    move :: a -> Vector -> a
    move_to :: a -> Point -> a