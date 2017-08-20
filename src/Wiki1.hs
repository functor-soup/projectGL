module Wiki1
  ( wiki1
  ) where

import Data.StateVar (($=), get)
import qualified Data.Vector.Storable as V
import Graphics.GL.Types (GLsizei)
import Graphics.Rendering.OpenGL.GL.CoordTrans (Size(..))
import Graphics.Rendering.OpenGL.GL.FlushFinish (flush)
import Graphics.Rendering.OpenGL.GL.Framebuffer
       (ClearBuffer(..), clear, clearColor)
import Graphics.Rendering.OpenGL.GL.PrimitiveMode
       (PrimitiveMode(..))
import Graphics.Rendering.OpenGL.GL.Shaders.Attribs
       (attribLocation)
import Graphics.Rendering.OpenGL.GL.Shaders.ProgramObjects
import Graphics.Rendering.OpenGL.GL.Shaders.ShaderObjects
       (Shader, ShaderType(..), compileShader, compileStatus,
        createShader, packUtf8, shaderSourceBS)
import Graphics.Rendering.OpenGL.GL.VertexArrays
       (Capability(..), DataType(..), VertexArrayDescriptor(..),
        drawArrays, vertexAttribArray, vertexAttribPointer)
import Graphics.Rendering.OpenGL.GL.VertexSpec
       (IntegerHandling(..))
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

name :: String
name = "Wiki Example 1"

w, h :: GLsizei
w = 300

h = 300

vertices :: V.Vector Float
vertices = V.fromList [0.0, 0.8, -0.8, -0.8, 0.8, -0.8]

-- =========================  SHADER STUFF  ==============================================
vertexShaderProg :: String
vertexShaderProg =
  mconcat
    [ "#version 120\n"
    , "attribute vec2 coord2d;"
    , "void main(void) {"
    , "  gl_Position = vec4(coord2d, 0.0, 1.0); "
    , "}"
    ]

fragmentShaderProg :: String
fragmentShaderProg =
  mconcat
    [ "#version 120\n"
    , "void main(void) {"
    , "gl_FragColor[0] = gl_FragCoord.x/640.0; "
    , "gl_FragColor[1] = gl_FragCoord.y/480.0; "
    , "gl_FragColor[2] = 0.5; "
    , "}"
    ]

shaders :: [(ShaderType, String)]
shaders =
  [(VertexShader, vertexShaderProg), (FragmentShader, fragmentShaderProg)]

compileShaderCode :: ShaderType -> String -> IO (Either String Shader)
compileShaderCode s t = do
  x <- createShader s
  (shaderSourceBS x) $= packUtf8 t
  compileShader x
  status <- get $ compileStatus x
  return $
    if status == True
      then Right x
      else Left "Shader failed to compile" -- should make this a little more specific

-- ======================   MAIN RENDERING PORTION =========================================
shaderPlayground :: Either String [Shader] -> IO ()
shaderPlayground z =
  case z of
    Left x -> putStrLn x >> leaveMainLoop
    Right k -> do
      p <- createProgram
      mapM_ (attachShader p) k
      linkProgram p
      currentProgram $= Just p
      l <- get (attribLocation p "coord2d")
      vertexAttribArray l $= Enabled
      V.unsafeWith vertices $ \ptr ->
        vertexAttribPointer l $= (ToFloat, VertexArrayDescriptor 2 Float 0 ptr)
      drawArrays Triangles 0 3
      vertexAttribArray l $= Disabled

-- Clear the background as white
clearScreen :: IO ()
clearScreen = do
  clearColor $= Color4 255 255 255 1
  clear [ColorBuffer]

simpleRender :: IO ()
simpleRender = do
  clearScreen
  mapM (uncurry compileShaderCode) shaders >>= return . sequence >>=
    shaderPlayground
  swapBuffers

-- ========================================================================================
onInput :: KeyboardCallback
onInput x _
  | x == 'q' = leaveMainLoop
  | otherwise = putStrLn "Press 'q' to exit "

callbacks :: IO ()
callbacks = do
  displayCallback $= simpleRender
  keyboardCallback $= Just onInput

wiki1 :: IO ()
wiki1 = do
  initialWindowSize $= Size w h
  initialDisplayMode $= [RGBAMode, DoubleBuffered]
  getArgsAndInitialize
  w <- createWindow name
  callbacks
  actionOnWindowClose $= MainLoopReturns
  mainLoop
  putStrLn "Bye bye"
