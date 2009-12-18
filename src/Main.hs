import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.SDL as SDL
import Data.Monoid

import qualified AnnotatedGraph as AG
import qualified Render

import qualified FRP.Yampa as Yampa

resX = 640 
resY = 480 

initScreen :: IO ()
initScreen = do
    SDL.init [SDL.InitTimer, SDL.InitVideo]
    -- resolution & color depth
    SDL.setVideoMode resX resY 32 [SDL.OpenGL]
    return ()


box :: Draw.Draw ()
box = Draw.scale 0.3 0.3 
        $ Draw.color (1,0,0,1) 
        $ Draw.convexPoly
            [(1,1),(1,-1),(-1,-1),(-1,1)]

drawing :: Draw.Draw [Char]
drawing = fmap (const "A") (Draw.color (0,0,1,0.5) box)
          `mappend`
          fmap (const "B") (Draw.translate (-0.1,0.2) box)



main :: IO ()
main = do
    initScreen
    Draw.draw drawing
    SDL.glSwapBuffers
    waitClicks
    SDL.quit
    return ()

    where
    waitClicks = do
        ev <- SDL.waitEvent
        case ev of
             SDL.Quit -> return ()
             SDL.MouseMotion x y _ _ -> do
                 let x' = 2*(fromIntegral x / fromIntegral resX) - 1
                 let y' = 1 - 2*(fromIntegral y / fromIntegral resY)
                 let tdraw = (Draw.translate (x',y') drawing)
                 --Draw.draw tdraw
                 --SDL.glSwapBuffers
                 waitClicks
             SDL.MouseButtonDown x y _ -> do
                 let x' = 2*(fromIntegral x / fromIntegral resX) - 1
                 let y' = 1 - 2*(fromIntegral y / fromIntegral resY)
                 let tdraw = (Draw.translate (x',y') drawing)
                 hit <- Draw.click (x',y') tdraw
                 case hit of
                      Nothing -> waitClicks
                      Just xs -> putStrLn xs >> waitClicks
             _ -> waitClicks


data AGEvent a b = AddNewNode a | RemoveNode Int | AddEdge b Int Int 

sdlToAGEvents :: Yampa.SF (Yampa.Event SDL.Event) (Yampa.Event (AGEvent String String))
sdlToAGEvents = proc sdlEvent -> do
                       let anGraphEvent = case (sdlEvent) of
                                      Yampa.Event (SDL.KeyDown ks) -> case (SDL.symKey ks) of
                                                          SDL.SDLK_a -> Yampa.Event (AddNewNode "new")
                                                          _          -> Yampa.NoEvent
                                      _ -> Yampa.NoEvent
                       Yampa.returnA -< anGraphEvent


eventToAG :: Yampa.SF (Yampa.Event (AGEvent a b), AG.AnnotatedGraph a b) (AG.AnnotatedGraph a b)
eventToAG = proc (anGraphEvent, ag) -> do 
                   let resAG = case anGraphEvent of
                                 Yampa.Event (AddNewNode x) -> AG.insNewLNode x ag
                                 _ -> ag
                   Yampa.returnA -< resAG
