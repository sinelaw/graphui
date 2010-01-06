import qualified Graphics.UI.SDL as SDL
import qualified Graphics.Rendering.OpenGL.GL as GL
import Control.Monad(when)

resX = 640
resY = 480

initScreen :: IO ()
initScreen = do
    SDL.init [SDL.InitTimer, SDL.InitVideo]
    -- resolution & color depth
    SDL.setVideoMode resX resY 32 [SDL.OpenGL]
    return ()

line :: (GL.GLdouble, GL.GLdouble) -> (GL.GLdouble, GL.GLdouble) -> IO ()
line (ax,ay) (bx,by) = do
    GL.renderPrimitive GL.Lines $ do
        GL.vertex $ GL.Vertex2 ax ay
        GL.vertex $ GL.Vertex2 bx by

drawing :: IO ()
drawing = line (-1,-1) (1,1)
    

main :: IO ()
main = do
    initScreen
--    GL.texture GL.Texture2D GL.$= GL.Enabled
    render
    SDL.quit
    return ()

    where render = do
            GL.clear [GL.ColorBuffer]
            GL.lineSmooth GL.$= GL.Enabled
            GL.blend GL.$= GL.Enabled
            GL.blendFunc GL.$= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
            GL.polygonSmooth GL.$= GL.Enabled
            GL.multisample GL.$= GL.Enabled
            GL.hint GL.LineSmooth GL.$= GL.Nicest
            GL.lineWidth GL.$= 2
            drawing
            SDL.glSwapBuffers
            ev <- SDL.waitEvent
            when (ev /= SDL.Quit) render

