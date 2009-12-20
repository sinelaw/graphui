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

draw1 :: Draw.Draw [Char]
draw1 = fmap (const "A") (Draw.color (0,0,1,0.5) box)

draw2 :: Draw.Draw [Char]
draw2 = fmap (const "B") (Draw.translate (-0.1,0.2) box)


sense :: Bool -> IO (Yampa.DTime, Maybe SDL.Event)
sense canBlock = do
    ev <- SDL.waitEvent
    return (0, Just ev)
    
actuate :: Bool -> Draw.Draw AG.Id -> IO Bool
actuate mayHaveChanged d = do           
  if mayHaveChanged then (Draw.draw d >> SDL.glSwapBuffers >> return False) else return False
  return False
  
initial :: IO SDL.Event
initial = SDL.waitEvent

main :: IO ()
main = do
  initScreen
  Yampa.reactimate initial sense actuate processor
  SDL.quit
  return ()


data AGEvent a b = AddNewNode a | RemoveNode Int | AddEdge b Int Int 

sdlToAGEvents :: Yampa.SF (SDL.Event) (Yampa.Event (AGEvent String String))
sdlToAGEvents = proc sdlEvent -> do
                  let anGraphEvent = case (sdlEvent) of
                                       SDL.KeyDown ks -> case (SDL.symKey ks) of
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

procRenderAG :: Yampa.SF (AG.AnnotatedGraph a b) (Draw.Draw AG.Id)
procRenderAG = proc ag -> do
             Yampa.returnA -< (Render.renderAG ag)


processor :: Yampa.SF SDL.Event (Draw.Draw AG.Id)
processor = proc sdlEvent -> do
              agEvent <- sdlToAGEvents -< sdlEvent
              rec ag <- eventToAG -< (agEvent, ag)
              d <- procRenderAG -< ag
              Yampa.returnA -< d
