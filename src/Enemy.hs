module Enemy where

import Graphics.Gloss
import KinematicBody

data Enemy = Enemy { rank :: EnemyRank, 
                     position :: Point,
                     direction :: Vector,
                     speed :: Float,
                     size :: Float }

data EnemyRank = Normal 
               | Better
               | Boss deriving (Eq)

instance KinematicBody Enemy where
    move e v = e { position = (position e) + v }
    move_to e pos = e { position = pos }