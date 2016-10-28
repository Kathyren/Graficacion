 import Graphics.Rendering.OpenGL
 import Graphics.UI.GLUT as GLUT
 --import Squares
 import Circle 
 import PointsForRendering
 main = do 
  (progName,_) <- getArgsAndInitialize 
  createWindow progName 
  displayCallback $= display 
  clearColor $= Color4 1 1 1 1 
  mainLoop
 display = do 
  clear [ColorBuffer,DepthBuffer] 
  loadIdentity 
  translate (Vector3 0 0 (-0.5::GLfloat))
  currentColor $= Color4 1 0 0 1 
  --square 1
  loadIdentity
  translate (Vector3 0.2 0.2 (0.5::GLfloat)) 
  currentColor $= Color4 0 0 1 1 
  fillCircle 0.5 
  flush

