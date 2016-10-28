import Graphics.UI.GLUT
import Data.IORef

_TIEMPO=0.1

main = do
      getArgsAndInitialize
      createWindow "Graficos"
      tiempo <- newIORef 0.0
      tras <- newIORef 0.0
      dir <- newIORef 0.0
      alt <- newIORef 0.0
      displayCallback $= desplegar tras
      --keyboardMouseCallback $= Just (trasladar dir)
      --keyboardMouseCallback $= Just (trasladar2 alt)
      idleCallback $= Just (temporizador tras tiempo dir alt)
      mainLoop

temporizador tras tiempo dire altu = do
        t <- get tiempo
        alt<-altu
        dir <- dire
        
        if t>=1.0 then do
                           f <- get tras
                           tras $=f + dir
                           
                           tiempo$= 0.0
                           postRedisplay Nothing
        else do
                            tiempo $= t+_TIEMPO
                            return ()

desplegar angulo = do 
      clear [ColorBuffer]
      translate $ Vector3 (0.0) (-0.25) (0.0::GLfloat)
      triangulo (-0.2::GLfloat, -0.2::GLfloat,0::GLfloat) (0.2::GLfloat, -0.2::GLfloat,0::GLfloat) (0::GLfloat, 0.2::GLfloat,0::GLfloat)
      loadIdentity
      flush
triangulo:: (GLfloat,GLfloat,GLfloat) ->  (GLfloat,GLfloat,GLfloat) -> (GLfloat,GLfloat,GLfloat)->IO()
triangulo  (x1,y1,z1) (x2,y2,z2) (x3,y3,z3)=  renderPrimitive Polygon $ do
            currentColor $= Color4 1 0 0 1
            vertex$Vertex3 x1 y1  (0::GLfloat)
            currentColor $= Color4 0 1 0 1
            vertex$Vertex3 x2 y2 (0::GLfloat)
            currentColor $= Color4 0 0 1 1
            vertex$Vertex3 x3 y3 (0::GLfloat)


trasladar  dir(SpecialKey KeyRight) Down _ _ = do
                      dir $= 0.1
trasladar  dir(SpecialKey KeyLeft) Down _ _ = do
                      dir $= -0.1
trasladar2  alt(SpecialKey KeyUp) Down _ _ = do
                      alt $= 0.1
trasladar2  alt(SpecialKey KeyDown) Down _ _ = do
                      alt $= -0.1

