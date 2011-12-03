-- lines beginning with ">" are executable
-- lines beginning with "<" are in the text,
-- lines beginning with "|" are also in the text,

module Picture ( Picture (Region, Over, EmptyPic),
                 Color (Black, Blue, Green, Cyan, 
                        Red, Magenta, Yellow, White),
                 regionToGRegion, shapeToGRegion, 
                 drawRegionInWindow, drawPic, draw, spaceClose,
                 module Region
                ) where
import Draw
import Region
import Data.Monoid
import SOE hiding (Region)
import qualified SOE as G (Region)

data Picture = Region Color Region
             | Picture `Over` Picture
             | EmptyPic
     deriving Show

instance Monoid Picture where
  mempty = EmptyPic
  mappend = Over

drawPic                 :: Window -> Picture -> IO ()
drawPic w (Region c r)   = drawRegionInWindow w c r
drawPic w (p1 `Over` p2) = do drawPic w p2; drawPic w p1
drawPic w EmptyPic       = return ()

drawRegionInWindow :: Window -> Color -> Region -> IO ()
drawRegionInWindow w c r 
  = drawInWindow w 
      (withColor c 
         (drawRegion (regionToGRegion r)))

regionToGRegion :: Region -> G.Region
regionToGRegion r = regToGReg (0,0) (1,1) r

regToGReg :: Vector -> Vector -> Region -> G.Region
type Vector = (Float,Float)

regToGReg loc sca (Shape s) 
  = shapeToGRegion loc sca s

regToGReg loc (sx,sy) (Scale (u,v) r)
  = regToGReg loc (sx*u,sy*v) r

regToGReg (lx,ly) (sx,sy) (Translate (u,v) r) 
  = regToGReg (lx+u*sx,ly+v*sy) (sx,sy) r

regToGReg loc sca Empty
  = createRectangle (0,0) (0,0)

regToGReg loc sca (r1 `Union` r2)
  = primGReg loc sca r1 r2 orRegion

regToGReg loc sca (r1 `Intersect` r2)
  = primGReg loc sca r1 r2 andRegion

regToGReg loc sca (r1 `Xor` r2)
  = primGReg loc sca r1 r2 xorRegion

regToGReg loc sca (Complement  r)
  = primGReg loc sca winRect r diffRegion

primGReg loc sca r1 r2 op
  = let gr1 = regToGReg loc sca r1
        gr2 = regToGReg loc sca r2
    in  op gr1 gr2

winRect :: Region
winRect = Shape (Rectangle 
                  (pixelToInch xWin) (pixelToInch yWin))

xWin2 = xWin `div` 2 
yWin2 = yWin `div` 2 


shapeToGRegion :: Vector -> Vector -> Shape -> G.Region
shapeToGRegion (lx,ly) (sx,sy) s
  = case s of
      Rectangle s1 s2  
        -> createRectangle (trans (-s1/2,-s2/2)) 
                           (trans (s1/2,s2/2))
      Ellipse r1 r2    
        -> createEllipse (trans (-r1,-r2))
                         (trans ( r1, r2))
      Polygon vs      
        -> createPolygon (map trans vs)
      RtTriangle s1 s2 
        -> createPolygon (map trans [(0,0),(s1,0),(0,s2)])
    where trans :: Vertex -> Point
          trans (x,y) = ( xWin2 + inchToPixel (lx+x*sx), 
                          yWin2 - inchToPixel (ly+y*sy) )

draw :: String -> Picture -> IO ()
draw s p
  = runGraphics $
    do w <- openWindow s (xWin,yWin)
       drawPic w p
       spaceClose w


xUnion :: Region -> Region -> Region
p1 `xUnion` p2 = (p1 `Intersect` Complement p2) `Union`
                 (p2 `Intersect` Complement p1)

r1 = Shape (Rectangle 3 2)
r2 = Shape (Ellipse 1 1.5)
r3 = Shape (RtTriangle 3 2)
r4 = Shape (Polygon [(-2.5,2.5), (-3.0,0), (-1.7,-1.0),
                     (-1.1,0.2), (-1.5,2.0)] )

reg1 = r3 `xUnion` (r1 `Intersect` Complement r2 `Union` r4)
pic1 = Region Blue reg1

reg2 = let circle = Shape (Ellipse 0.5 0.5)
           square = Shape (Rectangle 1 1)
       in (Scale (2,2) circle)
          `Union` (Translate (1,0) square)
          `Union` (Translate (-1,0) square)
pic2 = Region Yellow (Translate (0,-1) reg2)

pic3 = pic2 `Over` pic1

pic3book = pic3 `Over` Region White (Shape (Rectangle 6 5))

r = Shape (Rectangle 2 2)
pp1 = Region Red (Translate (1,1) (Scale (0.5,0.5) r))
pp2 = Region Blue (Scale (0.5,0.5) (Translate (1,1) r))

oneCircle   = Shape (Ellipse 1 1)
manyCircles = [ Translate (x,0) oneCircle | x <- [0,2..] ]
fiveCircles = foldr Union Empty (take 5 manyCircles)
fc = Region Red (Scale (0.25,0.25) fiveCircles)

r5 = let c1 = Shape (Ellipse 0.5 0.5)
         c2 = Translate (1,0) c1
         cs = Translate (1,1) (c1 `Union` c2)
     in Scale (0.5,0.5) cs
r6 = let c = Shape (Ellipse 0.5 0.5)
         s = Shape (Rectangle 1 1)
     in (Scale (2,2) c) `Union`
        ((Translate (2,0) s) `Union` 
         (Translate (-2,0) s))

pictToList :: Picture -> [(Color,Region)]
pictToList  EmptyPic      = []
pictToList (Region c r)   = [(c,r)]
pictToList (p1 `Over` p2) = pictToList p1 ++ pictToList p2

adjust :: [(Color,Region)] -> Coordinate -> 
          (Maybe (Color,Region), [(Color,Region)])


adjust regs p
  = case (break (\(_,r) -> r `containsR` p) regs) of
      (top,hit:rest) -> (Just hit, top++rest)
      (_,[])         -> (Nothing, regs)


loop :: Window -> [(Color,Region)] -> IO ()
loop w regs = 
    do clearWindow w
       sequence_ [ drawRegionInWindow w c r | (c,r) <- reverse regs ]
       (x,y) <- getLBP w
       case (adjust regs (pixelToInch (x - xWin2), 
                          pixelToInch (yWin2 - y) )) of
         (Nothing,  _      ) -> closeWindow w
         (Just hit, newRegs) -> loop w (hit : newRegs)

draw2 :: String -> Picture -> IO ()
draw2 s p
  = runGraphics $
    do w <- openWindow s (xWin,yWin)
       loop w (pictToList p)

p1,p2,p3,p4 :: Picture
p1 = Region Red r1
p2 = Region Blue r2
p3 = Region Green r3
p4 = Region Yellow r4

pic :: Picture 
pic = foldl Over EmptyPic [p1,p2,p3,p4]
main = draw2 "Picture Click Test" pic

p5 = Region White (Shape (Rectangle 6 5))
mainbook = draw2 "Picture Click Test" (pic `Over` p5)

