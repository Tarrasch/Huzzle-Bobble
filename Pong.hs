module Pong where

-- | I try a simple implementation of Pong using Fal

import SOE hiding (Region, Event)
import Fal
import Region
import Picture

go :: IO()
go = test $ pong 2

pong vel = walls `over` ballAndPaddles vel

walls = let upper = paint blue (translate ( 0,  1.7) (rec 4.4   0.05))
            lower = paint blue (translate ( 0, -1.7) (rec 4.4 (-0.05)))
        in  lower `over` upper
        
ballAndPaddles vel =  paddles `over` ball
  where pl = paddleHeight 'w' 's'
        pr = paddleHeight 'o' 'l'
        paddles = drawPaddle (-2.2) pl `over` drawPaddle (2.2) pr
        x  = integral dx
        y  = integral dy
        dx = vel `stepAccum` xbounce ->> negate
        dy = vel `stepAccum` ybounce ->> negate
        xbounce = let lb = x `between` (-2.3, -2.2) &&* y `between` (pl-0.3, pl+0.3)
                      rb = x `between` ( 2.2,  2.3) &&* y `between` (pr-0.3, pr+0.3)
                  in  when $ lb ||* rb
        
        ybounce = when $ y >* 1.7 ||* y <* (-1.7)
        ball = paint yellow $ translate (x, y) (ell 0.2 0.2)
        
x `between` (a,b) = x >* a &&* x <* b
        
-- Arg 1: Move-up-key
-- Arg 2: Move-down-key
paddleHeight :: Char -> Char -> Behavior Float
paddleHeight up dn = maxmin 1.4 (-1.4) $ 
        integral (fmap f            $ keyIsDown up)
      + integral (fmap (negate . f) $ keyIsDown dn)
   where f True         = 3.0
         f False        = 0.0
         maxmin ub lb v = lift2 min ub (lift2 max lb v) 
         
         
-- Arg 1: x-axis place (it's leftest coordinate)
-- Arg 2: Behavior of it's y-value
drawPaddle :: Float -> Behavior Float -> Behavior Picture
drawPaddle f bh = paint red $ translate (constB f, bh) (rec 0.05 0.6)



