{-# LANGUAGE Arrows #-}

module Main where

import qualified AnnotatedGraph as AG
import qualified Render

import WXFruit
import qualified FRP.Yampa as Yampa
import qualified Control.Arrow as Arrow
import qualified Graphics.UI.WX as WX

-- Elements of the game.

bg :: WXPicture
bg = wxWithColor WX.black wxfill

gameBox :: (Num t) => t -> t -> WX.Size2D t
gameBox w h = WX.sz w h

rect :: (Num a) => a -> a -> a -> a -> WX.Rect2D a
rect x y w h = WX.rect (WX.point x y) (WX.sz w h)

square :: (Num a) => a -> a -> a -> WX.Rect2D a
square x y a = rect x y a a

type Vec2 = (Double, Double)

infixl 0 `v2Add`
v2Add :: (Num t, Num u) => (t, u) -> (t, u) -> (t, u)
(x,y) `v2Add` (x',y') = (x+x', y+y')
infixl 1 `v2SMul`
v2SMul :: (Num a) => a -> (a, a) -> (a, a)
s `v2SMul` (x,y) = (s*x, s*y)


limitVec :: Vec2 -> Vec2 -> Vec2 -> Vec2
limitVec min max vec = let x' = if x <= mx then mx else x
                           y' = if y <= my then my else y
                           x'' = if x' >= mx' then mx' else x'
                           y'' = if y' >= my' then my' else y'
                           (mx, my) = min
                           (mx', my') = max
                           (x, y) = vec
                       in (x'', y'')
                        
dt :: Double
dt = 0.01

trackBall :: WXBox ([(Vec2, Vec2)], Vec2) [(Vec2, Vec2)]
trackBall = proc (balls, mpos) -> do
              let calcAcc pos = mpos `v2Add` (-1 `v2SMul` pos)
              let updateVel (px,py) vel acc = vel `v2Add` (dt `v2SMul` acc)
              let updatePos pos vel = limitVec (0,0) (300,300) (pos `v2Add` vel)
              let newBalls = map (\(v, p) -> (updateVel p v (calcAcc p), updatePos p v)) balls
              Yampa.returnA -< newBalls
                  
-- The top-level GUI: puts it all together.
guiMain :: WXGUI () ()
guiMain = wxHBox $ proc _ -> do
            mpos <- wxmouse -< ()
            rec 
                let x = fromIntegral $ WX.pointX mpos
                let y = fromIntegral $ WX.pointY mpos
                let ballPicS v (x',y') = wxWithColor WX.yellow $ wxPicFill $ wxellipse (square (round x') (round y') 10)
                let gamePic = foldr wxPicOver bg (map (uncurry ballPicS) nextBalls)
                nextBalls <- trackBall -< (curBalls, (x,y))
                curBalls <- ballsPre -< nextBalls
            _ <- wxpicture (psize (gameBox 350 350)) -< ppic gamePic
            Yampa.returnA -< ()
            where ballsPre = (wxBox . wxSF) $ Yampa.iPre [((0,0), (10,10)), ((0,0), (0,10)), ((0,0), (10,0))]

main :: IO ()
main = startGUI "Graphui" guiMain


data AGEvent a b = AddNewNode a | RemoveNode Int | AddEdge b Int Int


-- sdlToAGEvents :: Yampa.SF (Yampa.Event SDL.Event) (Yampa.Event (AGEvent String String))
-- sdlToAGEvents = proc sdlEvent -> do
--                        let anGraphEvent = case (inEvent sdlEvent) of
--                                       SDL.KeyDown ks -> case (SDL.symKey ks) of
--                                                           SDL.SDLK_a -> Yampa.Event (AddNewNode "new")
--                                                           _          -> Yampa.NoEvent
--                                       _ -> Yampa.NoEvent
--                        Yampa.returnA -< anGraphEvent
                                

eventToAG :: Yampa.SF (Yampa.Event (AGEvent a b), AG.AnnotatedGraph a b) (AG.AnnotatedGraph a b)
eventToAG = proc (anGraphEvent, ag) -> do 
                   let resAG = case anGraphEvent of
                                 Yampa.Event (AddNewNode x) -> AG.insNewLNode x ag
                                 _ -> ag
                   Yampa.returnA -< resAG