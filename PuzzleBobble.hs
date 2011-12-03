{-# LANGUAGE PackageImports #-}
module PuzzleBobble where


-- | I try a simple implementation of PuzzleBobble using Fal

import SOE hiding (Region, Event)
import FalMix
import Region
import Picture
import SevenSegment
import Data.Traversable (sequenceA)
import Control.Applicative ((<*>))
import Test.QuickCheck
import Data.Monoid 
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.Reader
import Control.Monad.State

-------------------------------- QuickStart --------------------------------

go :: IO()
go = test everything


testGun :: IO()
testGun = test $ drawGun `over` writeAngle

everything = drawGun `over` writeAngle `over` boarders `over` (constB $ boardToPicture exampleBoard1)


-------------------------------- Constants --------------------------------

maxRow, maxCol :: Int
maxRow = 8
maxCol = 6

leftMost, rightMost :: Float
leftMost  = -rightMost
rightMost = fromIntegral maxCol * ballRadius + ballRadius/2

topMost, bottomMost :: Float
topMost = 1.5
bottomMost = topMost - fromIntegral maxRow * ballDiameter

ballRadius, ballDiameter :: Float
ballRadius   = ballDiameter/2
ballDiameter = 0.5


-------------------------------- Derived Constants --------------------------------

roofCoordinates = [(x, 0) | x <- [0.. maxCol-1]]

-------------------------------- Types --------------------------------

type Degrees = Float

-- Nothing if empty,
-- Color if a marble is there
type Block = Maybe Color


-- (Inefficiently) using lists to represent the marbles on the screen.
--    _______________
-- 0  |()()()()()() |
-- 1  | ()()()()()()|
-- 2  |()()()()()() |
-- 3  | ()()()()()()|
-- 4  |()()()()()() |
-- r  | ()()()()()()|
--    ---------------
-- e   0011223344cc-
-- o   -0011223344cc

type Position = (Int, Int)
type Board = Map Position Color

data BobbleState = BobbleState { blocks :: Board,
                                 angle  :: Float,
                                 shot   :: Maybe ((Float, Float), Color)
                                 }


{-

data Color = Black
           | Blue
           | Green
           | Cyan
           | Red
           | Magenta
           | Yellow
           | White
  deriving (Eq, Ord, Bounded, Enum, Ix, Show, Read)
  
-}



-------------------------- Example boards and States -------------------------

takeColor :: Int -> Color
takeColor i' = [succ minBound .. maxBound] !! i
  where i = abs i' % length [succ minBound .. maxBound :: Color]


exampleBoard1 :: Board
exampleBoard1 = 
  Map.fromList [((x, y), takeColor (x)) | y <- [0..1], x <- [0..maxCol-1]]

-------------------------------- Boarderlines --------------------------------

makeBoarder :: Float -> Behavior Picture
makeBoarder xo = paint (constB Blue) $ translate (constB xo, 0.0) breg
  where breg = lift1 Shape $ lift2 Rectangle 0.05 3.0


leftBoarder, rightBoarder :: Behavior Picture
leftBoarder  = makeBoarder leftMost
rightBoarder = makeBoarder rightMost

roofBoarder :: Behavior Picture
roofBoarder = paint (constB Blue) $ translate (0, constB topMost) breg
  where breg = lift1 Shape $ lift2 Rectangle (constB $ rightMost - leftMost) 0.05


-- This is the only thing we need from boarderlines
boarders :: Behavior Picture
boarders = leftBoarder `over` rightBoarder `over` roofBoarder
  
-------------------------------- Gun --------------------------------

-- Given leftkey and rightkey
gunAngle :: Char -> Char -> Float -> Behavior Degrees
gunAngle l r a0 = maxmin pi 0.0 $ 
        constB a0 
      + integral (fmap f            $ keyIsDown l)
      + integral (fmap (negate . f) $ keyIsDown r)
   where f True         = 2.0
         f False        = 0.0
         maxmin ub lb v = lift2 min ub (lift2 max lb v) 

gun :: Float -> Behavior Degrees         
gun = gunAngle 'a' 'd'

myGunAngle :: Behavior Degrees         
myGunAngle = gun (pi/2)

writeAngle :: Behavior Picture
writeAngle = paint (constB Red) $ translate (2.5, -2.0) breg
  where breg = lift1 nToSegment n
        n    = fmap (round . (*(180/pi))) myGunAngle
   
drawGun :: Behavior Picture
drawGun = paint (constB Yellow) $ translate (0.0, -1.5) breg
  where breg = lift1 Shape bshp
        bshp = lift1 Polygon (sequenceA [bl, br, topr, topl])
          where bl   = constB (-0.2, 0); br = constB (0.2, 0)
                topr = let a = myGunAngle - 0.1 in pairB (cos a) (sin a)
                topl = let a = myGunAngle + 0.1 in pairB (cos a) (sin a)
                




-------------------------------- BubbleGeometry --------------------------------

-- Given a bubbles y x coordinate, return it's position
-- on the graphics layout
-- returning the middle-point of the ball
coordToPosition :: Position -> (Float, Float)
coordToPosition (x, y) | even y    = (x'*d + leftMost + r, topMost - r - y'*d * sin (pi/3))
                       | otherwise = let (rx, ry) = coordToPosition (x, (y-1))
                                     in  (rx + r, ry - d * sin (pi/3))
   where r = ballRadius; d = ballDiameter
         x' = fromIntegral x
         y' = fromIntegral y

dist :: (Float, Float) -> (Float, Float) -> Float
(x1, y1) `dist` (x2, y2) = sqrt $ dy*dy + dx*dx
  where dy = y2 - y1; dx = x2 - x1                   


adjacentPositions :: Position -> [(Int, Int)]
adjacentPositions (x0, y0) | even y0   = [(x, y) | 
                                          dx <- [-1..1], dy <- [-1..1],
                                          let x = x0 + dx, let y = y0 + dy,
                                          (dx, dy) `notElem` [(1, -1), (1, 1), (0,0)] ]
                           | otherwise = let pos1 = adjacentPositions (x0, (y0-1))
                                             pos2 = map incY pos1
                                         in         map adjust pos2
  where incY                      = fmap (+1)
        adjust (x, y) | even y    = (x + 1, y)
                      | otherwise = (x,     y)             
                                                            
-- Does the position exist in the grid?
legalPosition :: (Int, Int) -> Bool
legalPosition (x, y) = 0 <= x && 0 <= y && x < maxCol && y < maxRow


adjacentLegalPositions :: Position -> [(Int, Int)]
adjacentLegalPositions = filter legalPosition . adjacentPositions



roundPosition :: (Float, Float) -> Position
roundPosition fp = snd $ minimum list
  where list = [(d, p)    | x <- [0..maxCol-1],
                            y <- [0..maxRow-1],
                            let p = (x, y),
                            let d = fp `dist` coordToPosition p]


-------------------------------- Draw Board --------------------------------

boardToPicture :: Board -> Picture
boardToPicture = mconcat . map (uncurry marbleToPicture) . Map.toList

marbleToPicture :: Position -> Color -> Picture
marbleToPicture p c = Region c (unColoredMarble p)

unColoredMarble :: Position -> Region
unColoredMarble p = Translate (coordToPosition p) $ Shape circle
  where circle = Ellipse ballRadius ballRadius

-------------------------------- Add block --------------------------------

addBlock :: Board -> Position -> Color -> Board
addBlock blocks pos c = fallenRemoved
  where directRemoved = let found = snd $ runSearch blocks (search (==c) pos)
                        in  if Map.size found >= 3
                            then blocks \\ found
                            else blocks
        fallenRemoved = let ins = forM_ roofCoordinates (\pos -> search (const True) pos)
                            found = snd $ runSearch directRemoved ins                                      
                        in  found                        
        (\\)          = Map.difference
  
type SearchMonad = ReaderT Board (State Board)    
runSearch env = flip runState Map.empty .
                flip runReaderT env
  
search :: (Color -> Bool) -> (Int, Int) -> SearchMonad ()
search p pos = do bh <- beenHereOrEmpty pos
                  unless bh $
                    do Just c <- asks (Map.lookup pos)
                       modify $ Map.insert pos c
                       when (p c) $
                         do mapM_ (search p) (adjacentLegalPositions pos)
                         
  where when = Control.Monad.State.when                       
                    
                    
beenHereOrEmpty :: Position -> SearchMonad Bool
beenHereOrEmpty pos = do b1 <- gets (Map.member pos)
                         b2 <- asks (isNothing . Map.lookup pos)
                         return $ b1 || b2
                         

-------------------------------- Aiming State --------------------------------

aiming :: BobbleState -> Behavior BobbleState
aiming (BobbleState blocks angle Nothing) = first `untilB'` event
  where first    = lift3 BobbleState (constB blocks) gunAngle (constB Nothing)
        gunAngle = gun angle
        event    = charGotPressed 's' ->> shoot
        
                         
-------------------------------- ShootIsTraversing State --------------------------------

shoot :: BobbleState -> Behavior BobbleState
shoot (BobbleState blocks angle Nothing) = first `untilB'` undefined
  where first    = lift3 BobbleState (constB blocks) gunAngle (lift1 Just $ pairB ball $ constB Red)
        gunAngle = gun angle
        a'       = constB angle
        ball     = pairB (xpos) (integral $ sin a')
        xpos     = integral xvel
        xvel     = cos angle `stepAccum` xbounce ->> negate
        xbounce  = FalMix.when (xpos >* constB rightMost ||* xpos <* constB leftMost)
        roundPos = lift1 roundPosition ball
        neighbors = lift1 adjacentLegalPositions roundPos
        event    = firstOf realEvents
          where nlist      = lift1 unList (lift1 length neighbors) neighbors
                apply      = map . lift1  -- fmap . fmap :)
                existingNe = (filter . lift1) (flip Map.member blocks) neighbors
                fneighbors = apply coordToPosition existingNe
                distList   = map ((lift2 dist ball) <*>) fneighbors
                boolList   = apply (>ballRadius) distList
                empEvents  = map (FalMix.when) boolList
                realEvents = let myZip = zipWith snapshot_
                             in realEvents `myZip` existingNe
          {-
                apply      = lift1 . map  -- fmap . fmap :)
                existingNe = (lift1 . filter) (flip Map.member blocks) neighbors
                fneighbors = apply coordToPosition existingNe
                distList   = fmap fmap (lift1 dist ball) <*> fneighbors
                boolList   = apply (>ballRadius) distList
                realList   = unList boolList
          -}
                
                {-
aux :: Behavior (Position) -> Behavior (Float, Float) -> Event (Position)
aux bp bf = undefined
  where event = when . (>* constB ballRadius) $ (constB -$-
        -$-   = lift2 dist
-}

-------------------------------- Auxiliary --------------------------------

(%) = mod


-------------------------------- QuickCheck Properties --------------------------------


     
-- Property on equal distances between a -> b and a -> c if c and b are adjecent to a
prop_BubbleGeometry :: Int -> Int -> Bool
prop_BubbleGeometry x0 y0 = all (d=~) dists
                         && length dists == 5
  where x          = abs x0 % 10
        y          = abs y0 % 10
        self       = coordToPosition (x, y)
        neighbors  = map coordToPosition $ adjacentPositions (x, y)
        (d: dists) = map (dist self) neighbors
        a =~ b     = a < b + 1e-05 && b < a + 1e-05
                

prop_BubbleGeomInverse :: Position -> Bool
prop_BubbleGeomInverse (x0, y0) = p == invinv p
  where invinv = roundPosition . coordToPosition 
        p = (x0 % maxCol, y0 % maxRow)
                
