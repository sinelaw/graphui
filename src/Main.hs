{-# LANGUAGE Arrows #-}

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


type FloatType = GLdouble

resX :: Int
resX = 600
resY :: Int
resY = 600
fResX :: (RealFloat a) => a
fResX = fromIntegral resX
fResY :: (RealFloat a) => a
fResY = fromIntegral resY

initScreen :: IO ()
initScreen = do
    SDL.init [SDL.InitTimer, SDL.InitVideo]
    -- resolution & color depth
    _ <- SDL.setVideoMode resX resY 32 [SDL.OpenGL]
    --Draw.init
    return ()



getEvents :: IORef Yampa.Time -> IO (Yampa.DTime, Maybe SDL.Event)
getEvents tpRef = do
  tp <- readIORef tpRef
  ev <- SDL.waitEvent
--  SDL.delay 1
  t <- SDL.getTicks
  writeIORef tpRef (fromIntegral t)
  let dt = max 1 (fromIntegral t - tp)
  return (dt, Just ev)
    
  
sense :: IORef Yampa.Time -> Bool -> IO (Yampa.DTime, Maybe SDL.Event)
sense tpRef _ = do
  (dt, ev) <- getEvents tpRef
  return (dt, ev)
    


actuate :: (Show a, Eq a, Show b, Eq b, RealFloat c) => 
           Bool 
           -> (Bool, 
               Draw.Image (Maybe AG.Ids), 
               Yampa.Event (AGEvent a b), 
               AG.AnnotatedGraph a b c) 
           -> IO Bool
actuate mayHaveChanged (needQuit, draw, _, ag) = do
  when mayHaveChanged redraw'
  return needQuit
  
    where
      redraw' = if AG.renderGraph . AG.vrGraph $ ag
                then redraw
                else redrawMouse
      mpos = Vector2.getXY . Render.coordsFromSDL fResX fResY . AG.mousePos . AG.vrGraph $ ag
      cursor = Draw.translate (Render.onBoth realToFrac mpos) 
               Draw.%% Draw.scale 0.03 0.03 
               Draw.%% (Draw.tint (Draw.Color 0 1 0 1) . fmap (const Nothing) $ Render.circle)
      redraw      = Draw.clearRender (draw `mappend` cursor) >> SDL.glSwapBuffers
      redrawMouse = Draw.clearRender cursor >> SDL.glSwapBuffers
  


initial :: IO SDL.Event
initial = do
  initScreen
  return SDL.NoEvent

processor :: Yampa.SF SDL.Event (Bool,  -- shall we quit?
                                 Draw.Image (Maybe AG.Ids), -- The updated rendered screen
                                 Yampa.Event (AGEvent String String),  -- The event processed
                                 -- Debugging stuff:
                                 AG.AnnotatedGraph String String FloatType -- the resulting annotated graph
                                )
processor = proc sdlEvent -> do
              rec agEvent <- sdlToAGEvents -< (sdlEvent, draw)
                  ag <- eventToAG' -< agEvent
                  -- todo use accumHold or variants?
                  draw' <- procRenderAG -< ag
                  draw <- drawPre -< draw'
              needQuit <- guiEventHandler -< agEvent
              Yampa.returnA -< (needQuit, draw, agEvent, ag)
            where eventToAG' = sscanPrim' eventToAG AG.empty
                  drawPre = Yampa.iPre mempty

sscanPrim' :: (b -> a -> Maybe b) -> b -> Yampa.SF a b
sscanPrim' f b_init = Yampa.sscanPrim f' b_init b_init
    -- changes Just x into Just (x, x)
    where f' b a = case (f b a) of
            Nothing -> Nothing
            Just x -> Just (x,x)

main :: IO ()
main = do
  tpRef <- newIORef 0
  Yampa.reactimate initial (sense tpRef) actuate processor
  putStrLn "Quitting"
  SDL.quit
  return ()


data AGEvent a b = AddNewNode a 
                 -- | RemoveNode Int 
                 -- | AddEdge b Int Int 
                 | Quit 
                 | ToggleRender
                 | AGElementSelected AG.Ids 
                 | MouseMotion Int Int AG.Ids
                 | ZoomOut
                 | ZoomIn
                   deriving (Eq, Show)


--sdlToAGEvents :: Yampa.SF (SDL.Event, Draw.Image (Maybe AG.Ids)) (Yampa.Event (AGEvent String String))
sdlToAGEvents = proc (sdlEvent, draw) -> do
                  let anGraphEvent = case sdlEvent of
                                       SDL.KeyDown ks -> case (SDL.symKey ks) of
                                                           SDL.SDLK_a -> Yampa.Event . AddNewNode $ "new"
                                                           SDL.SDLK_r -> Yampa.Event ToggleRender
                                                           SDL.SDLK_EQUALS -> Yampa.Event ZoomIn
                                                           SDL.SDLK_MINUS  -> Yampa.Event ZoomOut
                                                           _          -> Yampa.NoEvent
                                       SDL.MouseButtonDown x y _ -> case Render.locateClick fResX fResY x y draw of 
                                                                      Nothing -> Yampa.NoEvent
                                                                      Just ids -> Yampa.Event (AGElementSelected ids)
                                       SDL.MouseMotion x y _ _ -> Yampa.Event (MouseMotion fx fy id')
                                           where fx = fromIntegral x
                                                 fy = fromIntegral y
                                                 id' = case Render.locateClick fResX fResY x y draw of
                                                         Nothing -> mempty
                                                         Just ids -> ids
                                       SDL.Quit -> Yampa.Event Quit
                                       _ -> Yampa.NoEvent
                  Yampa.returnA -< anGraphEvent


eventToAG :: (Show a, Eq a, RealFloat c) => AG.AnnotatedGraph a String c -> Yampa.Event (AGEvent a String)
                                            -> Maybe (AG.AnnotatedGraph a String c)
eventToAG ag (Yampa.Event (AddNewNode x)) = Just (Layout.autoLayout (AG.insNewLNode x ag))
eventToAG ag (Yampa.Event (MouseMotion x y ids)) = Just (AG.setMousePos mouseVec ag')
    where mouseVec = Vector2.vector2 (fromIntegral x) (fromIntegral y)
          ag' = AG.setHoveredElements ids ag
eventToAG ag (Yampa.Event (AGElementSelected id')) = Just (Layout.layoutIfNeeded (updatedSelectedElements id' ag))
 -- todo replace magic numbers
eventToAG ag (Yampa.Event ZoomIn ) = Just (AG.zoomBy 1.1 ag)
eventToAG ag (Yampa.Event ZoomOut) = Just (AG.zoomBy 0.9 ag)
eventToAG ag (Yampa.Event ToggleRender) = Just (AG.toggleRender ag)
eventToAG _ _ = Nothing


updatedSelectedElements :: (RealFloat c) => AG.Ids -> AG.AnnotatedGraph a String c -> AG.AnnotatedGraph a String c
updatedSelectedElements id' ag = if length nodesList == 2 then connectedAg else updatedAg
    where updatedAg = AG.setSelectedElements updatedSelected ag
          selectedList = Set.toList (getSelected ag) ++ Set.toList id'
          nodesList = [nid | nid <- selectedList, AG.idIsElement AG.Node nid]
          updatedSelected = Set.union id' (getSelected ag)
          getSelected = AG.selectedElements . AG.vrGraph
          connectedAg = AG.resetSelectedElements (AG.connectNodes nodesList "new" updatedAg)


procRenderAG :: Yampa.SF (AG.AnnotatedGraph a b FloatType) (Draw.Image (Maybe AG.Ids))
procRenderAG = proc ag -> do
             Yampa.returnA -< (Render.renderAG ag)


guiEventHandler :: (Eq a, Eq b) => Yampa.SF (Yampa.Event (AGEvent a b)) Bool
guiEventHandler = proc agEvent -> do
                    Yampa.returnA -< (agEvent == Yampa.Event Quit)


