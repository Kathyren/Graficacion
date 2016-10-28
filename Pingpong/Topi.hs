{-# LANGUAGE FlexibleContexts #-}
import Graphics.UI.GLUT
import Data.IORef

_TIEMPO=0.001

main = do
  getArgsAndInitialize
  initialDisplayMode $= [WithDepthBuffer, DoubleBuffered]
  createWindow "Graficos"
  depthFunc $= Just Less --3D
  windowSize $= Size 500 500
  windowPosition $= Position 300 100
  posX <- newIORef 0
  posY <- newIORef 0
  tiempo <- newIORef 0.0
  angulo <- newIORef 0.0
  matrixMode $= Projection
  loadIdentity  
  perspective 60 1 1 5
  lookAt (Vertex3 0 1.5 2) (Vertex3 0 0 0) (Vector3 0 1 0)
  --lookAt (Vertex3 0 0 (-2)) (Vertex3 0 0 0) (Vertex3 0 1 0)
  matrixMode $= Modelview 0
  displayCallback $= desplegar posX posY angulo
  passiveMotionCallback $= Just (raton posX posY)
  idleCallback $= Just (temporizador angulo tiempo)
  mainLoop

desplegar posX posY angulo = do          
   lighting $= Enabled
   px <- get posX
   py <- get posY
   let x = -1 + (fromIntegral px) / 250.0
   let y = -1 + (500.0-fromIntegral py) / 250.0
   position(Light 0)$=Vertex4 (x) (y) (-1) 1
   light (Light 0) $= Enabled
   clear [ColorBuffer, DepthBuffer]
   clear [ColorBuffer]
   a <- get angulo
   --translate $ Vector3 x y (0::GLfloat)         
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
                 angulo $= a+0.01                            
                 postRedisplay Nothing
              else do
                 tiempo $= t+_TIEMPO
                 return ()

--cubo :: GLfloat -> GLfloat -> IO()
cubo tam = do
  materialDiffuse Front$= Color4 0 1 0 1
  superficie [(0,0,0),(tam,0,0),(tam,tam,0),(0,tam,0)] (0,0,-1)
  materialDiffuse Front$= Color4 0 0 1 1
  superficie [(0,0,0),(0,tam,0),(0,tam,tam),(0,0,tam)] (-1,0,0)
  materialDiffuse Front$= Color4 0 1 0 1
  superficie [(0,0,tam),(tam,0,tam),(tam,tam,tam),(0,tam,tam)] (0,0,1)
  materialDiffuse Front$= Color4 1 1 1 1
  superficie [(0,0,0),(tam,tam,0),(tam,tam,tam),(tam,0,tam)](1,0,0)
  materialDiffuse Front$= Color4 1 1 0 1
  superficie [(0,0,0),(tam,0,0),(tam,0,tam),(0,0,tam)](0,-1,0)
  materialDiffuse Front$= Color4 1 0 1 1
  superficie [(0,tam,0),(tam,tam,0),(tam,tam,tam),(0,tam,tam)](0,1,0)

superficie :: [(GLfloat,GLfloat,GLfloat)]->(GLfloat,GLfloat,GLfloat)->IO()
superficie puntos (nx,ny,nz) = renderPrimitive Polygon $ do
     normal (Normal3 nx ny nz)
     mapM_ (\(x,y,z) -> vertex $ Vertex3 x y z) puntos

raton posX posY (Position x y) = do
    posX $= x
    posY $= y
    postRedisplay Nothing