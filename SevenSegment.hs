module SevenSegment (numberToSegment, nToSegment) where

import Picture
import Region
import Shape
import Data.List
import Data.Char
import Control.Applicative


-- Introduced Types
type Light = Bool

type Number = [Light]

-- Constants
scale :: Float
scale = 0.02

on  = True
off = False


-- The positioning of the lights.
{-

    a
    
f       b
        
    g   
        
e       c
        
    d
    
    
-}

vertBar, horiBar :: Float -> Region
vertBar scale = Shape $ Rectangle (scale) (scale*5)
horiBar scale = Shape $ Rectangle (scale*5) (scale)

toList a b c d e f g = [a, b, c, d, e, f, g]


-- Source: Wikipedia
intToNumber :: Char -> Number
intToNumber '0' = toList on   on   on   on   on   on   off
intToNumber '1' = toList off  on   on   off  off  off  off
intToNumber '2' = toList on   on   off  on   on   off  on 
intToNumber '3' = toList on   on   on   on   off  off  on 
intToNumber '4' = toList off  on   on   off  off  on   on 
intToNumber '5' = toList on   off  on   on   off  on   on 
intToNumber '6' = toList on   off  on   on   on   on   on 
intToNumber '7' = toList on   on   on   off  off  off  off
intToNumber '8' = toList on   on   on   on   on   on   on 
intToNumber '9' = toList on   on   on   on   off  on   on 
intToNumber '-' = toList off  off  off  off  off  off  on 
intToNumber x = error $ show x ++ " isn't a one-digit non-negative number."

-- a constant list, with the bars where the displays are geographically placed
barsPositions :: Float -> [Region]
barsPositions scale = let zipThat = zipWith ($) in map Translate offsets `zipThat` layouts
 where offsets = [(0, 2*d*0.85), (d, d), (d, -d), (0, -2*d*0.85), (-d, -d), (-d, d), (0, 0)]
        where dist = scale * 3.2
              d    = dist
       layouts = [horiBar, vertBar, vertBar, horiBar, vertBar, vertBar, horiBar] <*> [scale]


-- Given a "number" (list of Bools) and a scale, return the region
-- representing the number if the given "lights" were switched on.
drawNumber :: Number -> Float -> Region
drawNumber nbr scale = foldr (Union) Empty lights
  where lights = [bar | (b, bar) <- (nbr `zip` barsPositions scale), b]
  
  
-- Our important function seem from the outside world, given number n and a scale,
-- give the region representing the number n written with the given scale.  
numberToSegment :: Int -> Float -> Region
numberToSegment n scale = foldr (Union) Empty $ map aux $ ([0, -d ..] `zip` reverse (show n))
  where d = scale * 13
        aux (s, c) = Translate (s, 0) (drawNumber (intToNumber c) scale)
        

nToSegment :: Int -> Region
nToSegment = flip numberToSegment scale
        
-- test by running go
go = draw "abc" $ Region Red $ numberToSegment (-1234567890) scale
go2 scale = draw "abc" $ Region Red $ numberToSegment (-1234567890) scale





