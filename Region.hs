module Region ( Region (Shape, Translate, Scale, Complement, 
                        Union, Intersect, Xor, Empty),
                Coordinate,
                containsS, containsR, 
                module Shape
              ) where
import Shape

infixr 5 `Union`
infixr 6 `Intersect`

-- A Region is either:
data Region = Shape Shape		-- primitive shape
            | Translate Vector Region -- translated region
            | Scale      Vector Region -- scaled region
            | Complement Region	-- inverse of region
            | Region `Union` Region   -- union of regions
            | Region `Intersect` Region -- intersection of regions
            | Region `Xor` Region -- XOR of regions
            | Empty                   -- empty region
     deriving Show

type Vector = (Float,Float)





oneCircle   = Shape (Ellipse 1 1)
manyCircles = [ Translate (x,0) oneCircle | x <- [0,2..] ]




fiveCircles = foldr Union Empty (take 5 manyCircles)


r1 `difference` r2 = r1 `Intersect` (Complement r2)


type Coordinate = (Float,Float)







isLeftOf :: Coordinate -> Ray -> Bool
(px,py) `isLeftOf` ((ax,ay),(bx,by))
       = let (s,t) = (px-ax, py-ay)
             (u,v) = (px-bx, py-by)
         in  s*v >= t*u

type Ray = (Coordinate, Coordinate)








containsR :: Region -> Coordinate -> Bool

(Shape s) `containsR` p
   = s `containsS` p
(Translate (u,v) r) `containsR` (x,y)
   = let p = (x-u,y-v) in r `containsR` p
(Scale (u,v) r) `containsR` (x,y)
   = let p = (x/u,y/v) in r `containsR` p
(Complement r) `containsR` p 
   = not (r `containsR` p)
(r1 `Union` r2)     `containsR` p
   = r1 `containsR` p || r2 `containsR` p
(r1 `Intersect` r2) `containsR` p
   = r1 `containsR` p && r2 `containsR` p
(r1 `Xor` r2) `containsR` p
   = let a = r1 `containsR` p
         b = r2 `containsR` p
     in (a || b) && not (a && b)
Empty `containsR` p 
   = False

containsS :: Shape -> Coordinate -> Bool

(Rectangle s1 s2) `containsS` (x,y)
   = let t1 = s1/2; t2 = s2/2
     in (-t1<=x) && (x<=t1) && (-t2<=y) && (y<=t2)
(Ellipse r1 r2) `containsS` (x,y)
   = (x/r1)^2 + (y/r2)^2 <= 1
(Polygon pts) `containsS` p
   = let leftOfList = map (isLeftOf p) 
                          (zip pts (tail pts ++ [head pts]))
     in and leftOfList
(RtTriangle s1 s2) `containsS` p
   = (Polygon [(0,0),(s1,0),(0,s2)]) `containsS` p






univ = Complement Empty















