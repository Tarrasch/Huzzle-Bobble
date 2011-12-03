module FalMix where

-- | This module is a cleaned and improved version of Paul Hudaks
--   Fal-library from his book Haskell SOE. The improvements here
--   are done in the run-function (reactimate), where some in-built
--   features are implemented, like pause, time-flow-changing etc.
--
--   Rechanged again back to normal run-function

import SOE hiding (Region, Event)
import qualified SOE as G (Region, Event)
import Animation (picToGraphic)
import Shape
import Picture
import Memo1
import Control.Applicative
import Control.Monad
import Draw (xWin,yWin,intToFloat)
-- import Word (word32ToInt)
import Control.Concurrent.Chan

import Data.Foldable (Foldable)
import qualified Data.Foldable as F
import Data.Traversable (Traversable)
import qualified Data.Traversable as T

---------------------- Operators ----------------------

infixr 1 =>>, ->>
infixr 1 `untilB`, `switch`, `stepAccum`, `step`
infixl 0 .|.
infixr 4 <*, >*
infixr 3 &&*
infixr 2 ||*


---------------------- Types ----------------------

type Time = Float
type UserAction = G.Event


---------------------- New types ----------------------

newtype Behavior a 
  = Behavior (([Maybe UserAction],[Time]) -> [a])

newtype Event a 
  = Event (([Maybe UserAction],[Time]) -> [Maybe a])


---------------------- Library functions ----------------------

time :: Behavior Time
time = Behavior (\(_,ts) -> ts)

constB :: a -> Behavior a
constB x = Behavior (\_ -> repeat x)

($*) :: Behavior (a->b) -> Behavior a -> Behavior b
Behavior ff $* Behavior fb
  = Behavior (\uts -> zipWith ($) (ff uts) (fb uts))

lift0 :: a -> Behavior a
lift0 = constB

lift1 :: (a -> b) -> (Behavior a -> Behavior b)
lift1 f b1 
  = lift0 f $* b1

lift2 :: (a -> b -> c) -> (Behavior a -> Behavior b -> Behavior c)
lift2 f b1 b2 
  = lift1 f b1 $* b2

lift3 :: (a -> b -> c -> d) -> 
         (Behavior a -> Behavior b -> Behavior c -> Behavior d)
lift3 f b1 b2 b3 
  = lift2 f b1 b2 $* b3

lift4 :: (a -> b -> c -> d -> e) -> 
         (Behavior a -> Behavior b -> Behavior c -> Behavior d -> Behavior e)
lift4 f b1 b2 b3 b4 
  = lift3 f b1 b2 b3 $* b4
  
lift5 f b1 b2 b3 b4 b5
  = lift4 f b1 b2 b3 b4 $* b5
  
lift6 f b1 b2 b3 b4 b5 b6
  = lift5 f b1 b2 b3 b4 b5 $* b6

pairB :: Behavior a -> Behavior b -> Behavior (a,b)
pairB = lift2 (,)

unPair :: Behavior (a, b) -> (Behavior a, Behavior b)
unPair beh = (fstB beh, sndB beh)

fstB :: Behavior (a,b) -> Behavior a
fstB  = lift1 fst
sndB :: Behavior (a,b) -> Behavior b
sndB  = lift1 snd

paint :: Behavior Color -> Behavior Region -> Behavior Picture
paint = lift2 Region

red, blue, yellow, green, white, black :: Behavior Color
red    = lift0 Red
blue   = lift0 Blue
yellow = lift0 Yellow 
green  = lift0 Green
white  = lift0 White
black  = lift0 Black

shape :: Behavior Shape -> Behavior Region
shape   = lift1 Shape

ell, rec :: Behavior Float -> Behavior Float -> Behavior Region
ell x y = shape (lift2 Ellipse   x y) 
rec x y = shape (lift2 Rectangle x y)

translate :: (Behavior Float, Behavior Float) 
             -> Behavior Region -> Behavior Region
translate (Behavior fx, Behavior fy) (Behavior fp)
      = Behavior (\uts -> zipWith3 aux (fx uts) (fy uts) (fp uts))
        where aux x y p = Translate (x,y) p

(>*),(<*) :: Ord a => Behavior a -> Behavior a -> Behavior Bool
(>*) = lift2 (>)
(<*) = lift2 (<)

(&&*),(||*) :: Behavior Bool -> Behavior Bool -> Behavior Bool
(&&*) = lift2 (&&)
(||*) = lift2 (||)

over :: Behavior Picture -> Behavior Picture -> Behavior Picture
over = lift2 Over


---------------------- Instances ----------------------



instance Functor Behavior where
  fmap = lift1

instance Applicative Behavior where
  pure                        = lift0
  Behavior ff <*> Behavior fa = Behavior $ \uts -> zipWith ($) (ff uts) (fa uts)



-- nm im kinda unsure of Foldable/Traversable
{-
instance Foldable Behavior where
  foldMap = T.foldMapDefault

instance Traversable Behavior where
  sequenceA (Behavior f_a) = liftA2 undefined
-}

-- runBehavior (Behavior x) = x

instance Fractional a => Fractional (Behavior a) where
  (/) = lift2 (/)
  fromRational = lift0 . fromRational

instance Num a => Num (Behavior a) where
  (+) = lift2 (+)
  (*) = lift2 (*)
  negate = lift1 negate
  abs = lift1 abs
  signum = lift1 signum
  fromInteger = lift0 . fromInteger

instance Show (Behavior a)  where
  show b = "<< Behavior >>"

instance Eq (Behavior a) where
  a1 == a2 = error "Can't compare behaviors."

instance Floating a => Floating (Behavior a) where
  pi    = lift0 pi
  sqrt  = lift1 sqrt
  exp   = lift1 exp
  log   = lift1 log
  sin   = lift1 sin
  cos   = lift1 cos
  tan   = lift1 tan
  asin  = lift1 asin
  acos  = lift1 acos
  atan  = lift1 atan
  sinh  = lift1 sinh
  cosh  = lift1 cosh
  tanh  = lift1 tanh
  asinh = lift1 asinh
  acosh = lift1 acosh
  atanh = lift1 atanh
  
  

---------------------- General Functionality ----------------------
  
-- Behavior [a]  ==>
-- (UTS -> [[a]])
unList :: Int -> Behavior [a] -> [Behavior a]
--unList n (Behavior bxs) = [Behavior (\uts -> bxs uts !! i) | i <- [0..n-1]]
unList 0 (Behavior bxs) = []
unList n (Behavior bxs) = Behavior (\uts -> map head $ (bxs uts)) : unList (n - 1) (Behavior $ map tail . bxs)
    

---------------------- More library functions ----------------------

-- untilB, switch :: Behavior a -> Event (Behavior a) -> Behavior a

memoB :: Behavior a -> Behavior a
memoB (Behavior fb) = Behavior (memo1 fb)


untilB, switch :: Behavior a -> Event (Behavior a) -> Behavior a

Behavior fb `untilB` Event fe =
  memoB $ Behavior (\uts@(us,ts) -> loop us ts (fe uts) (fb uts))
    where loop (_:us) (_:ts) ~(e:es) (b:bs) =
            b : case e of 
                  Nothing             -> loop us ts es bs
                  Just (Behavior fb') -> fb' (us,ts)


Behavior fb `switch` Event fe =
  memoB $ Behavior (\uts@(us,ts) -> loop us ts (fe uts) (fb uts))
    where loop (_:us) (_:ts) ~(e:es) ~(b:bs) = 
            b : case e of 
                  Nothing             -> loop us ts es bs
                  Just (Behavior fb') -> loop us ts es (fb' (us,ts))

untilB' :: Behavior a -> Event (a -> Behavior a) -> Behavior a
Behavior fb `untilB'` Event fe =
  memoB $ Behavior (\uts@(us,ts) -> loop us ts (fe uts) (fb uts))
    where loop (_:us) (_:ts) ~(e:es) (b:bs) =
            b : case e of 
                  Nothing      -> loop us ts es bs
                  Just (f_fb') -> runBehavior (f_fb' b) (us,ts)
                  
runBehavior (Behavior x) = x

-- Left button pushed (mouse)
lbp :: Event ()
lbp = Event (\(uas,_) -> map getlbp uas)
      where getlbp (Just (Button _ True True)) = Just ()
            getlbp _                           = Nothing


while :: Behavior Bool -> Event ()
while (Behavior fb) 
  = Event (\uts -> map aux (fb uts))
    where aux True  = Just ()
          aux False = Nothing

unique :: (Show a, Eq a) => Event a -> Event a
unique (Event fe) =
      Event (\uts -> aux (fe uts))
      where aux xs = zipWith remdup (Nothing:xs) xs
            remdup x y | x==y      = Nothing
                       | otherwise = y

when :: Behavior Bool -> Event ()
when = unique . while

integral :: Behavior Float -> Behavior Float
integral (Behavior fb)
  = Behavior (\uts@(us,t:ts) -> 0 : loop t 0 ts (fb uts))
      where loop t0 acc (t1:ts) (a:as) 
                 = let acc' = acc + (t1-t0)*a
                   in acc' : loop t1 acc' ts as


withElem  :: Event a -> [b] -> Event (a,b)
withElem (Event fe) bs = Event (\uts -> loop (fe uts) bs)
  where loop (Just a  : evs) (b:bs) = Just (a,b) : loop evs bs
        loop (Nothing : evs)    bs  = Nothing    : loop evs bs

withElem_ :: Event a -> [b] -> Event b
withElem_ e bs = e `withElem` bs =>> snd

(.|.) :: Event a -> Event a -> Event a
Event fe1 .|. Event fe2 
  = Event (\uts -> zipWith aux (fe1 uts) (fe2 uts))
      where aux Nothing  Nothing  = Nothing
            aux (Just x) _        = Just x
            aux _        (Just y) = Just y


snapshot :: Event a -> Behavior b -> Event (a,b)
Event fe `snapshot` Behavior fb
  = Event (\uts -> zipWith' aux (fe uts) (fb uts))
      where aux (Just x) y = Just (x, y)
            aux Nothing  _ = Nothing
            zipWith' f ~(x:xs) ~(y:ys) = f x y : zipWith' f xs ys

snapshot_ :: Event a -> Behavior b -> Event b
snapshot_ e b = e `snapshot` b =>> snd


step :: a -> Event a -> Behavior a
a `step` e = constB a `switch` e =>> constB

stepAccum :: a -> Event (a->a) -> Behavior a
a `stepAccum` e = b 
   where b = a `step` (e `snapshot` b =>> uncurry ($))

-- Takes an event and filter+transforms it 
transformEvents :: Event a -> (a -> Maybe b) -> Event b
transformEvents (Event fe) f = Event (map (>>= f) . fe) 

-- Filters events
filterEvents :: Event a -> (a -> Bool) -> Event a
filterEvents e f = transformEvents e f'
  where f' a = do guard $ f a 
                  return a
                  
                  
(=>>) :: Event a -> (a->b) -> Event b
e =>> f = transformEvents e (Just . f)

(->>) :: Event a -> b -> Event b
e ->> v = e =>> \_ -> v                  
                  

never :: Event a
never = Event $ \_ -> repeat Nothing

firstOf :: [Event a] -> Event a
firstOf =  foldr (.|.) never                  
                  
---------------------- Keyboard ----------------------


key :: Bool -> Event Char
key b = Event (\(uas,_) -> map getkey uas)
     where getkey (Just (Key ch down)) | down == b = Just ch
           getkey _                                = Nothing

keyPress :: Event Char
keyPress = key True

keyRelease :: Event Char
keyRelease = key False      
            
-- The return value is uninteresting for these two            
charGotPressed, charGotReleased :: Char -> Event Char
charGotPressed  c = filterEvents keyPress   (==c)
charGotReleased c = filterEvents keyRelease (==c)
            
keyIsDown :: Char -> Behavior Bool 
keyIsDown c =  lift0 False `untilB` ( charGotPressed  c ->>
              (lift0 True  `untilB` ( charGotReleased c ->> keyIsDown c)))

---------------------- Mouse ----------------------

mm :: Event Coordinate
mm = Event (\(uas,_) -> map getmm uas)
     where getmm (Just (MouseMove pt)) = Just (gPtToPt pt)
           getmm _                     = Nothing


gPtToPt :: (Int, Int) -> Coordinate
gPtToPt (x,y) = ( pixelToInch (x - 300)
                , pixelToInch (250 - y) )

pixelToInch  :: Int -> Float
pixelToInch n = intToFloat n / 100

mouse :: (Behavior Float, Behavior Float)

mouse = (fstB m, sndB m)
          where m = (0,0) `step` mm
          
          
          
---------------------- Additional ----------------------
          
-- Try making a static image          
test beh = reactimate "FAL Test" (lift1 picToGraphic beh)

---------------------- Library Imlementation of reactimate ----------------------


reactimate :: String -> Behavior Graphic -> IO ()
reactimate title franProg
  = runGraphics $
    do w <- openWindowEx title (Just (0,0)) (Just (xWin,yWin))
              drawBufferedGraphic
       (us,ts,addEvents) <- windowUser w
       addEvents
       let drawPic (Just g) = 
             do setGraphic w g
                quit <- addEvents
                if quit 
                  then return True
                  else return False
           drawPic Nothing  = return False
       let Event fe = sample `snapshot_` franProg
       run drawPic (fe (us,ts))
       closeWindow w
  where
    run f (x:xs) = do
      quit <- f x
      if quit
        then return ()
        else run f xs
    run f [] = return ()

sample :: Event ()
sample = Event (\(us,_) -> map aux us)
  where aux Nothing  = Just ()
        aux (Just _) = Nothing

windowUser :: Window -> IO ([Maybe UserAction], [Time], IO Bool)
windowUser w
  = do (evs, addEv) <- makeStream
       t0 <- timeGetTime
       let addEvents =
             let loop rt = do
                   mev <- maybeGetWindowEvent w
                   case mev of
                     Nothing -> return False
                     Just e  -> case e of
                        Key ' ' True -> return True
                        Closed -> return True
                        _ -> addEv (rt, Just e) >> loop rt
             in do t <- timeGetTime
                   let rt = w32ToTime (t-t0)
                   quit <- loop rt
                   addEv (rt, Nothing)
                   return quit
       return (map snd evs, map fst evs, addEvents)

w32ToTime t = intToFloat (fromInteger (toInteger t)) / 1000

makeStream :: IO ([a], a -> IO ())
makeStream = do
  ch <- newChan
  contents <- getChanContents ch
  return (contents, writeChan ch)
  
         
