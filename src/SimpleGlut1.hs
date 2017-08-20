module SimpleGlut1
  ( simpleGlut1
  ) where

import Data.StateVar (($=))
import Graphics.GL.Types (GLsizei)
import Graphics.Rendering.OpenGL.GL.CoordTrans (Size(..))
import Graphics.Rendering.OpenGL.GL.FlushFinish (flush)
import Graphics.Rendering.OpenGL.GL.Framebuffer
       (ClearBuffer(..), clear, clearColor)
import Graphics.Rendering.OpenGL.GL.VertexSpec (Color4(..))
import Graphics.UI.GLUT.Begin
       (ActionOnWindowClose(..), actionOnWindowClose, leaveMainLoop,
        mainLoop)
import Graphics.UI.GLUT.Callbacks.Window
       (KeyboardCallback, displayCallback, keyboardCallback)
import Graphics.UI.GLUT.Initialization
       (DisplayMode(..), getArgsAndInitialize, initialDisplayMode,
        initialWindowSize)
import Graphics.UI.GLUT.Window (createWindow, swapBuffers)

-- Simple example of using Glut
name :: String
name = "Yokoso!"

w, h :: GLsizei
w = 300

h = 300

-- Rendering function
simpleRender :: IO ()
simpleRender = do
  clearColor $= Color4 255 255 0 0.5
  clear [ColorBuffer]
  swapBuffers

onInput :: KeyboardCallback
onInput x _
  | x == 'q' = leaveMainLoop
  | otherwise = putStrLn $ "You pressed " ++ show x

callbacks :: IO ()
callbacks = do
  displayCallback $= simpleRender
  keyboardCallback $= Just onInput

simpleGlut1 :: IO ()
simpleGlut1 = do
  initialWindowSize $= Size w h
  initialDisplayMode $= [RGBAMode, DoubleBuffered]
  getArgsAndInitialize
  w <- createWindow name
  callbacks
  actionOnWindowClose $= MainLoopReturns
  mainLoop
  putStrLn "Bye bye"
