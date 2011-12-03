module TestFal where

-- | A module where i test some functionality of the Fal langauge

import SOE hiding (Region, Event)
import Fal
import Region
import Picture

go :: IO()
go = test $ paint red box

box = translate (x, y) (rec szx szy)
  where x   = integral (fmap f $ keyIsDown 'd') 
            + integral (fmap (neg . f) $ keyIsDown 'a')
        y   = integral (fmap f $ keyIsDown 'w')
            + integral (fmap (neg . f) $ keyIsDown 's')
        szx = constB 0.5
        szy = constB 0.5
        neg = (0-)
        f True  = 3.0
        f False = 0.0
