{-# language NamedFieldPuns #-}
module View where

import Graphics.Gloss
import Constants
import Helpers
import Model
import Enemy
import Player
import Projectile

view :: World -> IO Picture
view = return . viewPure

viewPure :: World -> Picture
viewPure (World p es ps _ _) = 
                    pictures $ (renderProjectiles ps) ++ (renderEnemies es) ++ [renderPlayer p] ++ [ui_background, enemies_on_scene, lives_left, score_text]
                        where ui_y = (-worldHeightHalf)
                              ui_background = translate 0 ui_y $ color (fromRGB 0 0 0 200) $ rectangleSolid 640 30
                              enemies_on_scene = translate (-300) ui_y $ renderText $ show (length es) ++ " enemies on scene"
                              lives_left = translate (-100) ui_y $ renderText $ (show (lives p)) ++ " / 3 lives"
                              score_text = translate 100 ui_y $ renderText $ (show $ floor $ score p) ++ " seconds survived"

renderPlayer :: Player -> Picture
renderPlayer (Player (x, y) _ s lives score animation_index playing_animation) = 
                    pictures [outer_player, outer_player_border, inner_player, inner_player_border] 
                          where player_color = if playing_animation && animation_index < 9
                                                        then getAnimationFrame animation_index
                                                        else player_starting_color
                                outer_player = translate x y $ color player_color $ rectangleSolid s s
                                outer_player_border = translate x y $ color white $ rectangleWire (s+1) (s+1)
                                inner_player = translate x y $ color (fromRGB 128 213  233 256) $ rectangleSolid 7 7
                                inner_player_border = translate x y $ color white $ rectangleWire (7+1) (7+1)

renderProjectiles :: [Projectile] -> [Picture]
renderProjectiles = map renderProjectile

renderProjectile :: Projectile -> Picture
renderProjectile (Projectile (x, y) _ _ s) = pictures [border, body]
                                            where body = translate x y $ color black $ circleSolid $ s
                                                  border = translate x y $ color white $ circleSolid $ (s+1)

renderEnemies :: [Enemy] -> [Picture]
renderEnemies = map renderEnemy

renderEnemy :: Enemy -> Picture
renderEnemy (Enemy Normal (x, y)  _ _ size) = pictures [border, body] 
                                                where body = translate x y $ color (light $ light red) $ circleSolid size
                                                      border = translate x y $ color black $ circleSolid (size+1)
renderEnemy (Enemy Better (x, y)  _ _ size) = pictures [border, body] 
                                                where body = translate x y $ color  (dark $ dim red) $ circleSolid size
                                                      border = translate x y $ color black $ circleSolid (size+1)

renderEnemy (Enemy Boss (x, y)  _ _ size) = pictures [border, body, emblem1, emblem2, cutout] 
                                                where body = translate x y $ color (fromRGB 29 15 61 256) $ circleSolid size
                                                      border = translate x y $ color black $ circleSolid (size+1)
                                                      emblem1 = translate x y $ rotate 45 $ color (fromRGB 255 248 178 50) $ rectangleSolid (size-10) (size-10) 
                                                      emblem2 = translate x y $ rotate 90 $ color (fromRGB 255 248 178 50) $ rectangleSolid (size-14) (size-14)
                                                      cutout = translate x y $ color worldColor $ circleSolid (size-35)

renderText :: String -> Picture
renderText s = scale textSize textSize $ color white $ text s

textSize :: Float
textSize = 0.08

getAnimationFrame :: Int -> Color
getAnimationFrame i = animationFrames !! i

animationFrames :: [Color]
animationFrames = [white, black, (fromRGB 255 248 178 256), (fromRGB 247 113 29 256), (fromRGB 246 61 10 230), (fromRGB 107 56 44 150), (fromRGB 100 59 48 130), (fromRGB 38 24 13 100), (fromRGB 53 19 8 150)]