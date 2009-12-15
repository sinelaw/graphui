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

-- The top-level GUI: puts it all together.
guiMain :: WXGUI () ()
guiMain = wxHBox $ proc _ -> do
            mpos <- wxmouse -< ()
            let x = WX.pointX mpos
            let y = WX.pointY mpos
            let ballPicS = wxWithColor WX.yellow $ wxPicFill $ wxellipse (square x y 10)
            let gamePic = ballPicS `wxPicOver` bg
            _ <- wxpicture (psize (gameBox 200 200)) -< ppic gamePic
            Yampa.returnA -< ()

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