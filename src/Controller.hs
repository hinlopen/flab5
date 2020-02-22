module Controller where

import Graphics.Gloss
import Graphics.Gloss.Data.Vector
import System.Random
import Constants
import Helpers
import Model
import KinematicBody
import Player
import Enemy
import Projectile

step :: Float -> World -> IO World
step dt world
    | paused world = return world
    | spawn_cd world - dt <= 0 =
        do rndx <- randomRIO(-worldWidthHalf, worldWidthHalf)
           rndy <- randomRIO(-worldHeightHalf, worldHeightHalf)                        
           rndvx <- randomRIO(-1.0, 1.0)
           rndvy <- randomRIO(-1.0, 1.0)
           rndc <- randomRIO(0, 1.0)

           return $ world { enemies = (spawnEnemy rndx rndy rndvx rndvy rndc) : (enemies world),
                                      spawn_cd = 0.2 }

    | otherwise = return $ updateWorld $ checkCollisions $ moveWorld dt world

moveWorld :: Float -> World -> World
moveWorld dt world = world { enemies     = map (moveEnemy (player world)) (enemies world),
                             projectiles = moveProjectiles (projectiles world),
                             player      = clampToBorders $ (player world) { score = (score (player world)) + dt }, 
                             spawn_cd    = spawn_cd world - dt }

--Move all projectiles and delete those off screen boundaries
moveProjectiles :: [Projectile] -> [Projectile]
moveProjectiles ps = map moveProjectile (filter onScreen ps)
                  where moveProjectile p = move p (mulSV (Projectile.speed p) (Projectile.direction p))

moveEnemy :: Player -> Enemy -> Enemy
moveEnemy _ e@(Enemy Normal _   dir s _) = move e (mulSV s dir)
moveEnemy p e@(Enemy _ pos dir s _) = move e (mulSV s new_dir)
                                                where follow_strength = 0.1
                                                      new_dir = normalizeV ((mulSV (1-follow_strength) dir) + mulSV follow_strength ((Player.position p) - pos))

checkCollisions :: World -> World
checkCollisions world = world { enemies = not_colliding}
                          where not_colliding = filter (detectCollision (projectiles world)) (enemies world)

updateWorld :: World -> World
updateWorld world = if playing_animation (player world)
                       then if animation_index (player world) > 9
                              then world { player = (player world) { playing_animation = False}}
                              else world { player = (player world) { animation_index = (animation_index (player world)) + 1} }
                       else world

spawnEnemy :: Float -> Float->  Float->  Float -> Float -> Enemy
spawnEnemy x y vx vy r = Enemy (enemy_rank r) (x, y) (normalizeV (vx, vy)) (0.3*(2 + (5-2)*(1-r))) (enemy_size (enemy_rank r))

enemy_rank :: Float -> EnemyRank
enemy_rank a | a < 0.5  = Boss
             | a < 0.3   = Better
             | otherwise = Normal

enemy_size :: EnemyRank -> Float
enemy_size r | r == Normal = 9
             | r == Better = 17
             | otherwise   = 50

clampToBorders :: Player -> Player
clampToBorders p = p { Player.position = (clamp (-worldWidthHalf+s) (worldWidthHalf-s) (fst (Player.position p)), 
                                         (clamp (-worldHeightHalf+s) (worldHeightHalf-s) (snd (Player.position p)))) }
                                              where s = Player.size p / 2

shoot :: Point -> World -> World
shoot mouse_pos world = world { player = (player world) { animation_index = 0, playing_animation = True},
                                projectiles = new_projectiles ++ (projectiles world) }
                                    where player_pos = Player.position (player world)
                                          dir = normalizeV $ mouse_pos - player_pos
                                          new_projectiles = [(Projectile player_pos (shotgun_dir x) 20 2) | x <- [-n..n]]
                                          n = 5
                                          shotgun_dir x = rotateV (x*4/360) dir

blast :: World -> World
blast world = world { player = (player world) { animation_index = 0, playing_animation = True},
                      projectiles = new_projectiles ++ (projectiles world) }
                            where new_projectiles = [(Projectile (Player.position (player world)) (shotgun_dir x) 10 10) | x <- [-n..n]]
                                  n = 20
                                  delta = (2*n) / pi
                                  shotgun_dir x = rotateV (x*delta) (1,0)