import Graphics.UI.GLUT
import Data.IORef

_TIEMPO=0.01
main = do
  getArgsAndInitialize
  initalDisplayMode $= [withDeptBuffer, DoubleBuffered]
  createWindow "Graficos"
  depthFunc $= Just Less
  windowSize $= Size 500 500
  windowPosition $= Position 300 100
  tiempo <- newIORef 0.0
  angulo <- newIORef 0.0
  posX <- newIORef 0
  posY <- newIORef 0
  displayCallback $= desplegar posX posY angulo
  keyboardMouseCallback $= Just (raton posX posY)
  idleCallback $= Just (temporizador angulo tiempo)
  mainLoop
  --tamX (size tx _) = tx
  --tamY (size _ ty) = ty

desplegar posX posY angulo = do 
         clear [ColorBuffer]
         a <- get angulo
         px <- get posX
         py <- get posY
         let x = -1 + (fromIntegral px) / 250.0
         let y = -1 + (500.0-fromIntegral py) / 250.0
         translate $ Vector3 x y (0::GLfloat)
         rotate a $ Vector3 0 0 (1::GLfloat)
         translate $ Vector3 (-0.25) (-0.25) (0::GLfloat)       
         recuadro (0,0,0) (0.5,0.5,0)
         loadIdentity
         --perservingMatrix do 
         --swapBuffers
         flush

raton posX posY (MouseButton LeftButton) Down _ (Position x y) = do
        posX $= x
        posY $= y
        postRedisplay Nothing
raton _ _ _ _ _ _ = return ()

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
cubo =
  currentColor$= Color4 1 0 0 1
  superficie [(0,0,0),(tam,0,0),(tam,tam,0),(0,tam,0)]
  currentColor$= Color4 1 0 0 1
  superficie [(0,0,0),(0,tam,0),(0,tam,tam),(0,0,tam)]
  currentColor$= Color4 1 0 0 1
  superficie [(0,0,tam),(tam,0,tam),(tam,tam,tam),(0,tam,tam)]
  currentColor$= Color4 1 0 0 1
  superficie [(0,0,0),(tam,tam,0),(tam,tam,tam),(tam,0,tam)]
  currentColor$= Color4 1 1 0 1
  superficie [(0,0,0),(tam,0,0),(tam,0,tam),(0,0,tam)]
  currentColor$= Color4 1 0 0 1
  superficie [(0,tam,0),(tam,tam,0),(tam,tam,tam),(0,tam,tam)]


superficie puntos = do
  renderPrimitive Polygon$do
     mapM_ (\(x,y,z) -> vertex $ Vertex3 x y z) puntos
