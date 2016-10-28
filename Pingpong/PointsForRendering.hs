module PointsForRendering where
import Graphics.UI.GLUT 
import Graphics.Rendering.OpenGL

renderInWindow displayFunction = do 
  (progName,_) <-  getArgsAndInitialize
  createWindow progName
  displayCallback $= displayFunction
  mainLoop

displayPoints points primitiveShape = do 
  renderAs primitiveShape points
  flush

--makeVertexes = mapM_ (\(x,y,z) -> vertex $ Vertex3 x y z)

renderAs figure ps = renderPrimitive figure$mapM_ (\(x,y,z) -> vertex $ Vertex3 x y z) ps


