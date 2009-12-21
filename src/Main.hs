import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.SDL as SDL

import qualified AnnotatedGraph as AG
import qualified Render
import qualified Layout

import qualified System.IO.Unsafe

import qualified Data.Set as Set

import qualified FRP.Yampa as Yampa
import Control.Monad(when)

resX :: Int
resX = 640 
resY :: Int
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
    
actuate :: (Show a, Eq a, Show b, Eq b) => Bool -> (Bool, Draw.Draw AG.Id, AGEvent a b, AG.AnnotatedGraph a b) -> IO Bool
actuate mayHaveChanged (needQuit, d, agEvent, ag) = do
--    print ag
    when (agEvent /= NoEvent) (print agEvent)
    when (not needQuit && mayHaveChanged) redraw
    return needQuit
  where
    redraw = Draw.draw d >> SDL.glSwapBuffers
  
initial :: IO SDL.Event
initial = do
  initScreen
  ev <- SDL.waitEvent
  return ev

processor :: Yampa.SF SDL.Event (Bool,  -- shall we quit?
                                 Draw.Draw AG.Id, -- The updated rendered screen
                                 -- Debugging stuff:
                                 AGEvent String String,  -- The event processed
                                 AG.AnnotatedGraph String String -- the resulting annotated graph
                                )
processor = proc sdlEvent -> do
              rec agEvent <- sdlToAGEvents -< (sdlEvent, draw)
                  ag <- eventToAG -< (agEvent, preAg)
                  -- todo use accumHold or variants?
                  laidOutAg <- layoutAG -< ag
                  preAg <- agIPre -< laidOutAg
                  draw <- procRenderAG -< ag
              needQuit <- guiEventHandler -< agEvent
              Yampa.returnA -< (needQuit, draw, agEvent, laidOutAg)
            where agIPre = Yampa.iPre AG.empty

main :: IO ()
main = do
  Yampa.reactimate initial sense actuate processor
  putStrLn "Quitting"
  SDL.quit
  return ()


data AGEvent a b = AddNewNode a | RemoveNode Int | AddEdge b Int Int | Quit | NoEvent | AGElementSelected AG.Id
                   deriving (Eq, Show)

convCoords :: (Integral a, Fractional b) => a -> a -> (b, b)
convCoords x y = (2*(fromIntegral x / fromIntegral resX) - 1, 1 - 2*(fromIntegral y / fromIntegral resY))

locateClick :: (Integral a) => a -> a -> Draw.Draw AG.Id -> AG.Id
locateClick x y draw = unMaybeID . System.IO.Unsafe.unsafePerformIO $ (getIds x y draw)
    where getIds x' y' draw' =  do
            let pos = (convCoords x' y')
            res <- Draw.click pos draw'
            return res
          unMaybeID (Just id) = id
          unMaybeID Nothing = AG.Id Set.empty

sdlToAGEvents :: Yampa.SF (SDL.Event, Draw.Draw AG.Id) (AGEvent String String)
sdlToAGEvents = proc (sdlEvent, draw) -> do
                  let anGraphEvent = case (sdlEvent) of
                                       SDL.KeyDown ks -> case (SDL.symKey ks) of
                                                           SDL.SDLK_a -> AddNewNode "new"
                                                           _          -> NoEvent
                                       SDL.MouseButtonDown x y _ -> AGElementSelected (locateClick x y draw)
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


guiEventHandler :: (Eq a, Eq b) => Yampa.SF (AGEvent a b) Bool
guiEventHandler = proc agEvent -> do
                    Yampa.returnA -< (agEvent == Quit)

layoutAG :: Yampa.SF (AG.AnnotatedGraph a b) (AG.AnnotatedGraph a b)
layoutAG = proc ag -> do
             Yampa.returnA -< (Layout.autoLayout ag)