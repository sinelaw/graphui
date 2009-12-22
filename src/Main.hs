import qualified Math.Vector2 as Vector2
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.SDL as SDL

import qualified AnnotatedGraph as AG
import qualified Render
import qualified Layout

import qualified System.IO.Unsafe -- to hide the dot-layout stuff

import qualified Data.Set as Set

import qualified FRP.Yampa as Yampa
import Control.Monad(when)
import Data.Monoid(Monoid(..))

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
    
actuate :: (Show a, Eq a, Show b, Eq b) => Bool -> (Bool, Draw.Draw AG.Ids, Yampa.Event (AGEvent a b), AG.AnnotatedGraph a b) -> IO Bool
actuate mayHaveChanged (needQuit, d, agEvent, ag) = do
    print ag
    when (agEvent /= Yampa.NoEvent) (print . Yampa.fromEvent $ agEvent)
    when (not needQuit && mayHaveChanged) redraw
    return needQuit
  where
    (x,y) = Vector2.vector2XY . AG.mousePos . AG.vrGraph $ ag
    cursor = (Draw.translate (convCoords (x) (y)) (Render.nodeBox 123))
    redraw = Draw.draw (cursor `mappend` d) >> SDL.glSwapBuffers
  
initial :: IO SDL.Event
initial = do
  initScreen
  ev <- SDL.waitEvent
  return ev

processor :: Yampa.SF SDL.Event (Bool,  -- shall we quit?
                                 Draw.Draw AG.Ids, -- The updated rendered screen
                                 -- Debugging stuff:
                                 Yampa.Event (AGEvent String String),  -- The event processed
                                 AG.AnnotatedGraph String String -- the resulting annotated graph
                                )
processor = proc sdlEvent -> do
              rec agEvent <- sdlToAGEvents -< (sdlEvent, draw)
                  ag <- eventToAG -< (agEvent, preAg)
                  -- todo use accumHold or variants?
                  laidOutAg <- layoutAG -< ag
                  preAg <- agIPre -< laidOutAg
                  draw <- procRenderAG -< preAg
              needQuit <- guiEventHandler -< agEvent
              Yampa.returnA -< (needQuit, draw, agEvent, laidOutAg)
            where agIPre = Yampa.iPre AG.empty

main :: IO ()
main = do
  Yampa.reactimate initial sense actuate processor
  putStrLn "Quitting"
  SDL.quit
  return ()


data AGEvent a b = AddNewNode a | RemoveNode Int | AddEdge b Int Int | Quit | AGElementSelected AG.Ids | MouseMotion Int Int
                   deriving (Eq, Show)

convCoords :: Double -> Double -> (Double, Double)
convCoords x y = (2*(x / fromIntegral resX) - 1, 1 - 2*(y / fromIntegral resY))

locateClick :: (Integral a) => a -> a -> Draw.Draw AG.Ids -> Maybe AG.Ids
locateClick x y draw = System.IO.Unsafe.unsafePerformIO $ (getIds (fromIntegral x) (fromIntegral y) draw)
    where getIds x' y' draw' =  do
            let pos = (convCoords x' y')
            res <- Draw.click pos draw'
            return res

sdlToAGEvents :: Yampa.SF (SDL.Event, Draw.Draw AG.Ids) (Yampa.Event (AGEvent String String))
sdlToAGEvents = proc (sdlEvent, draw) -> do
                  let anGraphEvent = case (sdlEvent) of
                                       SDL.KeyDown ks -> case (SDL.symKey ks) of
                                                           SDL.SDLK_a -> Yampa.Event . AddNewNode $ "new"
                                                           _          -> Yampa.NoEvent
                                       SDL.MouseButtonDown x y _ -> case locateClick x y draw of 
                                                                      Nothing -> Yampa.NoEvent
                                                                      Just id' -> Yampa.Event (AGElementSelected id')
                                       SDL.MouseMotion x y _ _ -> Yampa.Event (MouseMotion (fromIntegral x) (fromIntegral y))
                                       SDL.Quit -> Yampa.Event Quit
                                       _ -> Yampa.NoEvent
                  Yampa.returnA -< anGraphEvent


eventToAG :: (Show a, Eq a) => Yampa.SF (Yampa.Event (AGEvent a String), AG.AnnotatedGraph a String) (AG.AnnotatedGraph a String)
eventToAG = proc (anGraphEvent, ag) -> do 
                   let resAG = case anGraphEvent of
                                 Yampa.NoEvent -> ag
                                 Yampa.Event (AddNewNode x) -> AG.setNeedsLayout True (AG.insNewLNode x ag) 
                                 Yampa.Event (MouseMotion x y) -> AG.setMousePos mouseVec ag
                                                                  where mouseVec = (Vector2.vector2 (fromIntegral x) (fromIntegral y))
                                 Yampa.Event (AGElementSelected id') -> updatedSelectedElements id' ag
                                 _ -> ag
                   Yampa.returnA -< resAG

updatedSelectedElements :: AG.Ids -> AG.AnnotatedGraph a String -> AG.AnnotatedGraph a String
updatedSelectedElements id' ag = if (Set.size nodes == 2) then (AG.connectNodes nodes "new" updatedAg) else updatedAg
    where updatedAg = AG.setSelectedElements updatedSelected ag
          nodes = Set.filter (AG.idIsElement AG.Node) updatedSelected
          updatedSelected = (Set.union id' (getSelected ag))
          getSelected = AG.selectedElements . AG.vrGraph 
          

procRenderAG :: Yampa.SF (AG.AnnotatedGraph a b) (Draw.Draw AG.Ids)
procRenderAG = proc ag -> do
             Yampa.returnA -< (Render.renderAG ag)


guiEventHandler :: (Eq a, Eq b) => Yampa.SF (Yampa.Event (AGEvent a b)) Bool
guiEventHandler = proc agEvent -> do
                    Yampa.returnA -< (agEvent == Yampa.Event Quit)

layoutAG :: Yampa.SF (AG.AnnotatedGraph a b) (AG.AnnotatedGraph a b)
layoutAG = proc ag -> do
             let laidOutAG = case (AG.needsLayout . AG.vrGraph $ ag) of
                               True -> (Layout.autoLayout ag)
                               False -> ag
             Yampa.returnA -< laidOutAG