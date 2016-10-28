import Graphics.UI.GLUT
import Data.IORef

_TIEMPO=0.01

main = do
  getArgsAndInitialize
  initialDisplayMode $= [WithDepthBuffer, DoubleBuffered]
  createWindow "Graficos"
  depthFunc $= Just Less
  windowSize $= Size 500 500
  windowPosition $= Position 300 100
  tiempo <- newIORef 0.0
  angulo <- newIORef 0.0
  displayCallback $= desplegar angulo
  --keyboardMouseCallback $= Just (raton posX posY)
  idleCallback $= Just (temporizador angulo tiempo)
  mainLoop
  --tamX (size tx _) = tx
  --tamY (size _ ty) = ty

desplegar angulo = do 
         clear [ColorBuffer, DepthBuffer]
         clear [ColorBuffer]
         a <- get angulo         
         preservingMatrix $ do
           rotate a $ Vector3 1 1 (1::GLfloat)
           translate $ Vector3 (-0.25) (-0.25) (0::GLfloat)
           cubo 0.5
         swapBuffers


temporizador angulo tiempo = do
        a <- get angulo
        t <- get tiempo
        if t>=1.0 then do
                            tiempo $= t+_TIEMPO
                            angulo $= a+0.1                            
                            postRedisplay Nothing
                      else do
                            tiempo $= t+_TIEMPO
                            return ()

recuadro (x ,y ,z) (x2, y2, z2) = renderPrimitive Polygon $ do
            currentColor $= Color4 1 0 0 1
            vertex$Vertex3 x y (0::GLfloat)
            vertex$Vertex3 x2 y (0::GLfloat)
            currentColor $= Color4 0.2 0 0 1
            vertex$Vertex3 x2 y2  (0::GLfloat)
            vertex$Vertex3 x y2 (0::GLfloat)
cubo :: GLfloat -> IO()
cubo tam = do
  currentColor$= Color4 0 1 0 1--verde
  superficie [(0,0,0),(tam,0,0),(tam,tam,0),(tam/2,tam,tam/2)]
  currentColor$= Color4 0 0 1 1--azul
  superficie [(0,0,0),(tam/2,tam,tam/2),(0,tam,tam)]
  currentColor$= Color4 1 0 0 1--roja
  superficie [(0,0,tam),(tam,0,tam),(tam/2,tam,tam/2)]
  currentColor$= Color4 1 1 1 1
  superficie [(0,0,0),(tam/2,tam,tam/2),(tam,0,tam)]
  currentColor$= Color4 1 1 0 1
  superficie [(0,0,0),(tam,0,0),(0,0,tam)]
  currentColor$= Color4 1 0 1 1
  --superficie [(0,tam,0),(tam,tam,0),(tam,tam,tam)]

superficie :: [(GLfloat,GLfloat,GLfloat)]->IO()
superficie puntos = do
  renderPrimitive Polygon$do
     mapM_ (\(x,y,z) -> vertex $ Vertex3 x y z) puntos
