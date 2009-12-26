import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.SDL as SDL

import qualified AnnotatedGraph as AG
import qualified Render
import qualified Layout
import qualified Math.Vector2 as Vector2

import qualified Data.Set as Set

import qualified FRP.Yampa as Yampa

import Control.Monad(when)
import Data.Monoid(Monoid(..))

import Data.IORef (IORef, newIORef, readIORef, writeIORef)

import Graphics.Rendering.OpenGL.GL(GLdouble)
import qualified Graphics.Rendering.OpenGL.GL as GL

type FloatType = GLdouble

resX :: Int
resX = 640
resY :: Int
resY = 480
fResX :: (RealFloat a) => a
fResX = fromIntegral resX
fResY :: (RealFloat a) => a
fResY = fromIntegral resY

initScreen :: IO ()
initScreen = do
    SDL.init [SDL.InitTimer, SDL.InitVideo]
    -- resolution & color depth
    _ <- SDL.setVideoMode resX resY 32 [SDL.OpenGL]
    Draw.draw Draw.empty
    return ()



getEvents :: IORef Yampa.Time -> IO (Yampa.DTime, Maybe SDL.Event)
getEvents tpRef = do
  tp <- readIORef tpRef
  ev <- SDL.pollEvent
  SDL.delay 1
  t <- SDL.getTicks
  writeIORef tpRef (fromIntegral t)
  let dt = max 1 (fromIntegral t - tp)
  return (dt, Just ev)
    
  
sense :: IORef Yampa.Time -> Bool -> IO (Yampa.DTime, Maybe SDL.Event)
sense tpRef _ = do
  (dt, ev) <- getEvents tpRef
  return (dt, ev)
    


actuate :: (Show a, Eq a, Show b, Eq b, RealFloat c) => 
           IORef Yampa.Time 
           -> Bool 
           -> (Bool, 
               Draw.Draw AG.Ids, 
               Yampa.Event (AGEvent a b), 
               AG.AnnotatedGraph a b c) 
           -> IO Bool
actuate trRef mayHaveChanged (needQuit, draw, _, ag) = do
  --print ag
  --when (Yampa.isEvent agEvent) (print . Yampa.fromEvent $ agEvent)
  tp <- readIORef trRef
  t <- fmap fromIntegral SDL.getTicks
  let dt = t - tp
  when (mayHaveChanged && (dt > 30)) redraw'
    
  return needQuit
  
    where
      redraw' = do if (AG.renderGraph . AG.vrGraph $ ag) 
                     then redraw
                     else redrawMouse
                   t'' <- SDL.getTicks
                   writeIORef trRef (fromIntegral t'')
      mpos = Vector2.getXY . Render.coordsFromSDL fResX fResY . AG.mousePos . AG.vrGraph $ ag
      cursor = (Draw.translate (Render.onBoth realToFrac mpos) (Render.nodeBox 0.01 0.01 123))
      redraw      = {-# SCC "redraw" #-} fastDraw (cursor `mappend` draw) >> SDL.glSwapBuffers
      redrawMouse = {-# SCC "redraw'" #-} fastDraw (cursor) >> SDL.glSwapBuffers
  


fastDraw :: Draw.Draw a -> IO ()
fastDraw d = do
  GL.clear [GL.ColorBuffer]
  Draw.runDrawing d
    
initial :: IO SDL.Event
initial = do
  initScreen
  Draw.init
  GL.texture GL.Texture2D GL.$= GL.Enabled
  GL.blend GL.$= GL.Enabled
  GL.blendFunc GL.$= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
  GL.lineSmooth GL.$= GL.Enabled
  GL.lineWidth GL.$= 1
  GL.hint GL.LineSmooth GL.$= GL.Nicest
  return SDL.NoEvent

processor :: Yampa.SF SDL.Event (Bool,  -- shall we quit?
                                 Draw.Draw AG.Ids, -- The updated rendered screen
                                 Yampa.Event (AGEvent String String),  -- The event processed
                                 -- Debugging stuff:
                                 AG.AnnotatedGraph String String FloatType -- the resulting annotated graph
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
  tpRef <- newIORef 0
  trRef <- newIORef 0
  Yampa.reactimate initial (sense tpRef) (actuate trRef) processor
  putStrLn "Quitting"
  SDL.quit
  return ()


data AGEvent a b = AddNewNode a 
                 -- | RemoveNode Int 
                 -- | AddEdge b Int Int 
                 | Quit 
                 | ToggleRender
                 | AGElementSelected AG.Ids 
                 | MouseMotion Int Int
                 | ZoomOut
                 | ZoomIn
                   deriving (Eq, Show)


sdlToAGEvents :: Yampa.SF (SDL.Event, Draw.Draw AG.Ids) (Yampa.Event (AGEvent String String))
sdlToAGEvents = proc (sdlEvent, draw) -> do
                  let anGraphEvent = case (sdlEvent) of
                                       SDL.KeyDown ks -> case (SDL.symKey ks) of
                                                           SDL.SDLK_a -> Yampa.Event . AddNewNode $ "new"
                                                           SDL.SDLK_r -> Yampa.Event ToggleRender
                                                           SDL.SDLK_EQUALS -> Yampa.Event ZoomIn
                                                           SDL.SDLK_MINUS  -> Yampa.Event ZoomOut
                                                           _          -> Yampa.NoEvent
                                       SDL.MouseButtonDown x y _ -> case Render.locateClick fResX fResY x y draw of
                                                                      Nothing -> Yampa.NoEvent
                                                                      Just id' -> Yampa.Event (AGElementSelected id')
                                       SDL.MouseMotion x y _ _ -> Yampa.Event (MouseMotion fx fy)
                                           where fx = fromIntegral x
                                                 fy = fromIntegral y
                                       SDL.Quit -> Yampa.Event Quit
                                       _ -> Yampa.NoEvent
                  Yampa.returnA -< anGraphEvent


eventToAG :: (Show a, Eq a, RealFloat c) => Yampa.SF (Yampa.Event (AGEvent a String), AG.AnnotatedGraph a String c) 
                                                     (AG.AnnotatedGraph a String c)
eventToAG = proc (anGraphEvent, ag) -> do
                   let resAG = case anGraphEvent of
                                 Yampa.NoEvent -> ag
                                 Yampa.Event (AddNewNode x) -> AG.setNeedsLayout True (AG.insNewLNode x ag)
                                 Yampa.Event (MouseMotion x y) -> AG.setMousePos mouseVec ag
                                                                  where mouseVec = (Vector2.vector2 (fromIntegral x) (fromIntegral y))
                                 Yampa.Event (AGElementSelected id') -> updatedSelectedElements id' ag
                                 Yampa.Event ZoomIn  -> AG.zoomBy 1.1 ag -- todo replace magic numbers
                                 Yampa.Event ZoomOut -> AG.zoomBy 0.9 ag --
                                 Yampa.Event ToggleRender -> AG.toggleRender ag
                                 _ -> ag
                   Yampa.returnA -< resAG

updatedSelectedElements :: (RealFloat c) => AG.Ids -> AG.AnnotatedGraph a String c -> AG.AnnotatedGraph a String c
updatedSelectedElements id' ag = if (length nodesList == 2) then connectedAg else updatedAg
    where updatedAg = AG.setSelectedElements updatedSelected ag
          selectedList = Set.toList (getSelected ag) ++ Set.toList id'
          nodesList = [nid | nid <- selectedList, AG.idIsElement AG.Node nid]
          updatedSelected = (Set.union id' (getSelected ag))
          getSelected = AG.selectedElements . AG.vrGraph
          connectedAg = AG.resetSelectedElements (AG.connectNodes nodesList "new" updatedAg)


procRenderAG :: Yampa.SF (AG.AnnotatedGraph a b FloatType) (Draw.Draw AG.Ids)
procRenderAG = proc ag -> do
             Yampa.returnA -< (Render.renderAG ag)


guiEventHandler :: (Eq a, Eq b) => Yampa.SF (Yampa.Event (AGEvent a b)) Bool
guiEventHandler = proc agEvent -> do
                    Yampa.returnA -< (agEvent == Yampa.Event Quit)

layoutAG :: (RealFloat c) => Yampa.SF (AG.AnnotatedGraph a b c) (AG.AnnotatedGraph a b c)
layoutAG = proc ag -> do
             let laidOutAG = case (AG.needsLayout . AG.vrGraph $ ag) of
                               True -> (Layout.autoLayout ag)
                               False -> ag
             Yampa.returnA -< laidOutAG