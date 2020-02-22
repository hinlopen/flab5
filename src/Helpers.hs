module Helpers where

import Graphics.Gloss

clamp :: (Ord a) => a -> a -> a -> a
clamp mi ma = max mi . min ma

fromRGB :: Float -> Float -> Float -> Float -> Color
fromRGB r g b a = makeColor (r/256.0) (g/256.0) (b/256.0) (a/256.0)