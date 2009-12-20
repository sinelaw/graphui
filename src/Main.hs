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




sense :: Bool -> IO (Yampa.DTime, Maybe SDL.Event)
sense canBlock = do
    ev <- SDL.waitEvent
    return (0, Just ev)
    
actuate :: (Show a, Eq a, Show b, Eq b) => Bool -> (Bool, Draw.Draw AG.Id, AGEvent a b) -> IO Bool
actuate mayHaveChanged (needQuit, d, agEvent) = do
  putStrLn (show agEvent)
  putStrLn ("need to quit = " ++ show needQuit)
  if needQuit 
    then return True
    else if mayHaveChanged 
           then (Draw.draw d >> SDL.glSwapBuffers >> return False) 
           else return False
  return False
  
initial :: IO SDL.Event
initial = do
  initScreen
  ev <- SDL.waitEvent
  return ev

processor :: Yampa.SF SDL.Event (Bool, Draw.Draw AG.Id, AGEvent String String)
processor = proc sdlEvent -> do
              agEvent <- sdlToAGEvents -< sdlEvent
              rec ag <- eventToAG -< (agEvent, preAg)
                  -- todo use accumHold or variants?
                  preAg <- agIPre -< ag
              d <- procRenderAG -< ag
              needQuit <- guiEventHandler -< agEvent
              Yampa.returnA -< (needQuit, d, agEvent)
            where agIPre = Yampa.iPre AG.empty

main :: IO ()
main = do
  Yampa.reactimate initial sense actuate processor
  SDL.quit
  return ()


data (Show a, Show b, Eq a, Eq b) => AGEvent a b = AddNewNode a | RemoveNode Int | AddEdge b Int Int | Quit | NoEvent
                   deriving (Eq, Show)

sdlToAGEvents :: Yampa.SF (SDL.Event) (AGEvent String String)
sdlToAGEvents = proc sdlEvent -> do
                  let anGraphEvent = case (sdlEvent) of
                                       SDL.KeyDown ks -> case (SDL.symKey ks) of
                                                           SDL.SDLK_a -> AddNewNode "new"
                                                           _          -> NoEvent
                                       SDL.Quit -> Quit
                                       _ -> NoEvent
                  Yampa.returnA -< anGraphEvent


eventToAG :: (Show a, Eq a, Show b, Eq b) => Yampa.SF (AGEvent a b, AG.AnnotatedGraph a b) (AG.AnnotatedGraph a b)
eventToAG = proc (anGraphEvent, ag) -> do 
                   let resAG = case anGraphEvent of
                                 AddNewNode x -> AG.insNewLNode x ag
                                 _ -> ag
                   Yampa.returnA -< resAG

procRenderAG :: Yampa.SF (AG.AnnotatedGraph a b) (Draw.Draw AG.Id)
procRenderAG = proc ag -> do
             Yampa.returnA -< (Render.renderAG ag)


guiEventHandler :: (Show a, Eq a, Show b, Eq b) => Yampa.SF (AGEvent a b) Bool
guiEventHandler = proc agEvent -> do
                    Yampa.returnA -< (agEvent == Quit)