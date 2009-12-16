{-# LANGUAGE Arrows #-}

module Main where

import qualified AnnotatedGraph as AG
import qualified Render

import qualified FRP.Yampa as Yampa
import qualified FRP.Yampa.Vector2 as Vector2
import qualified Control.Arrow as Arrow
import WXFruit
import qualified Graphics.UI.WX as WX

-- Elements of the game.

bg :: WXPicture
bg = wxWithColor WX.black wxfill

gameBox :: (Num t) => t -> t -> WX.Size2D t
gameBox w h = WX.sz w h


guiRender :: (RealFloat t) => WXBox (AG.AnnotatedGraph a b, Vector2.Vector2 t) WXPicture
guiRender = proc (ag, mpos) -> do
              let gamePic = Render.overlay [Render.renderAG ag, Render.drawCircle 10 mpos, bg]
              Yampa.returnA -< gamePic

-- The top-level GUI: puts it all together.
guiMain :: WXGUI () ()
guiMain = wxHBox $ proc _ -> do
            mpos <- wxmouse -< ()
            rec newAg <- eventToAG -< (Yampa.NoEvent, ag)
                ag <- graphDelay -< newAg
                gamePic <- guiRender -< (newAg, Vector2.vector2 (fromIntegral . WX.pointX $ mpos) 
                                                                (fromIntegral . WX.pointY $ mpos))
                
            _ <- wxpicture (psize (gameBox 350 350)) -< ppic gamePic
            Yampa.returnA -< ()
    where graphDelay = (wxBox . wxSF) $ Yampa.iPre AG.empty

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
                                

eventToAG :: WXBox (Yampa.Event (AGEvent a b), AG.AnnotatedGraph a b) (AG.AnnotatedGraph a b)
eventToAG = proc (anGraphEvent, ag) -> do 
                   let resAG = case anGraphEvent of
                                 Yampa.Event (AddNewNode x) -> AG.insNewLNode x ag
                                 _ -> ag
                   Yampa.returnA -< resAG