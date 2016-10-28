import Graphics.UI.GLUT
 

 
main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  _window <- createWindow "Hello World"
  displayCallback $= display
  mainLoop
 
dibPelota :: DisplayCallback
dibPelota = do 
  renderPrimitive Polygon $
     mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z) pelota
  flush

pelota :: [(GLfloat,GLfloat,GLfloat)]
pelota = [ (sin (2*pi*k/12), cos (2*pi*k/12), 0) | k <- [1..12] ]

generarCirculo = [ (sin (2*pi*k/12), cos (2*pi*k/12), 0) | k <- [1..12] ]
 