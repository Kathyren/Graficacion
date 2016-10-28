

import Graphics.UI.GLUT
import Data.IORef
_TIEMPO=0.01
main = do
      getArgsAndInitialize
      createWindow "Graficos"
      tiempo <- newIORef 0.0
      angulo <- newIORef 0.0
      escala <- newIORef 1.0
      
      displayCallback $= desplegar angulo
      idleCallback $= Just (temporizador angulo tiempo)
      mainLoop

temporizador angulo tiempo = do
        a <- get angulo
        t <- get tiempo
        if t>=1.0 then do
                            trasladar giro
                            angulo $= a+0.1
                            tiempo $= 0.0
                            postRedisplay Nothing
                      else do
                            tiempo $= t+_TIEMPO
                            return ()
desplegar tras = do 
                        clear [ColorBuffer]
                        currentColor $= Color4 0 0 0 1
                        e<-get tras
                        loadIdentity
                        translate $ Vector3 0 e (0.0 ::GLfloat)
                        triangulo (0,0.8,0) (-0.5,0,0) (0.5,0,0)
                        flush

cuadro t =  renderPrimitive Polygon $ do
            currentColor $= Color4 1 0 0 1
            vertex$Vertex3 0 0 (0::GLfloat)
            vertex$Vertex3 0 t (0::GLfloat)
            currentColor $= Color4 0.2 0 0 1
            vertex$Vertex3 t t (0::GLfloat)
            vertex$Vertex3 t 0 (0::GLfloat)

triangulo:: (GLfloat,GLfloat,GLfloat) ->  (GLfloat,GLfloat,GLfloat) -> (GLfloat,GLfloat,GLfloat)->IO()
triangulo  (x1,y1,z1) (x2,y2,z2) (x3,y3,z3)=  renderPrimitive Polygon $ do
            currentColor $= Color4 1 0 0 1
            vertex$Vertex3 x1 y1  (0::GLfloat)
            currentColor $= Color4 0 1 0 1
            vertex$Vertex3 x2 y2 (0::GLfloat)
            currentColor $= Color4 0 0 1 1
            vertex$Vertex3 x3 y3 (0::GLfloat)

teclado escala (Char '+') Down _ _ = do
      e <- get escala
      escala $= e+0.1
      postRedisplay Nothing
teclado escala (Char '-') Down _ _ = do
      e <- get escala
      escala $= e-0.1
      postRedisplay Nothing
teclado _ _ _ _ _ = return ()

teclad escala angulo tecla Down _ _ = case tecla of 
     (SpecialKey KeyRight) -> do 
                               a<- get angulo
                               angulo$= a-5
                               postRedisplay Nothing
     (SpecialKey KeyLeft) -> do 
                               a<- get angulo 
                               angulo$= a+5
                               postRedisplay Nothing
     (SpecialKey KeyUp) -> do 
                               a<- get angulo
                               angulo$= a-5
                               postRedisplay Nothing
     (SpecialKey KeyDown) -> do 
                               a<- get angulo 
                               angulo$= a+5
                               postRedisplay Nothing
     (Char '+') -> do
                     e <- get escala
                     escala $= e+0.1
                     postRedisplay Nothing
     (Char '-') -> do
                     e <- get escala
                     escala $= e-0.1
                     postRedisplay Nothing
    
     _ -> return () 
teclad _ _ _ _ _ _ = return ()

trasladar  giro(SpecialKey KeyRight) Down _ _ = do
                      giro $= 0.1
trasladar  giro(SpecialKey KeyLeft) Down _ _ = do
                      giro $= -0.1
trasladar  giro(SpecialKey KeyUp) Down _ _ = do
                      giro $= 0.1
trasladar  giro(SpecialKey KeyDown) Down _ _ = do
                      giro $= -0.1

