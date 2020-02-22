module Model where

import Graphics.Gloss
import Constants
import Player
import Enemy
import Projectile

data GameState = Playing | Paused deriving (Eq, Bounded, Enum)

data World = World { player :: Player,
                     enemies :: [Enemy],
                     projectiles :: [Projectile],
                     spawn_cd :: Float,
                     game_state :: GameState
               }      

initialWorld :: World
initialWorld = World initialPlayer [] [] 1.5 Playing 

initialPlayer :: Player
initialPlayer = Player (0,0) 30 player_size starting_lives 0 0 False

paused :: World -> Bool
paused world = (game_state world) == Paused

detectCollision :: [Projectile] -> Enemy -> Bool
detectCollision ps e = not (any (aabbCollision e) ps)

-- From https://noonat.github.io/intersect/
aabbCollision :: Enemy -> Projectile -> Bool
aabbCollision a b = 
    let ax = fst $ Enemy.position a
        ay = snd $ Enemy.position a
        bx = fst $ Projectile.position b
        by = snd $ Projectile.position b
        as = Enemy.size a /2
        bs = Projectile.size b / 2
    in 
    if (bs + as - (abs(ax-bx)) <= 0)
      then False;
      else if (bs + as - (abs (by - ay)) <= 0)
              then False
              else True

-- Check if the projectile is on screen, with an offset to prevent deletion while it's still partially on screen.
onScreen :: Projectile -> Bool
onScreen (Projectile (x,y) _ _ _) = x > -worldWidthHalf  - k &&
                                      x < worldWidthHalf   + k &&
                                      y > -worldHeightHalf - k &&
                                      y < worldHeightHalf  + k
                                          where k = 200