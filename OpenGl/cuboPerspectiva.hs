{-# LANGUAGE FlexibleContexts #-}
import Graphics.UI.GLUT
import Data.IORef

_TIEMPO=0.0001

main = do
  getArgsAndInitialize
  initialDisplayMode $= [WithDepthBuffer, DoubleBuffered]
  createWindow "Graficos"
  depthFunc $= Just Less
  windowSize $= Size 500 500
  windowPosition $= Position 300 100
  tiempo <- newIORef 0.0
  posX <- newIORef 0
  posY <- newIORef 0
  angulo <- newIORef 0.0
  c <- newIORef 1

  displayCallback $= desplegar posX posY  c angulo
  passiveMotionCallback$= Just(movRaton posX posY)
    --Just(raton posX posY c)
  keyboardMouseCallback $= Just (raton c)
  idleCallback $= Just (temporizador angulo tiempo)
  
--perspective 60 1 ce 5
  --lookAt (Vertex3 0 0 2) (Vertex3 0 0 0) (Vector3 0 1 0)
  --matrixMode$= Modelview 0
  --lighting $= Enabled
  --position (Light 0) $= Vertex4 (equis) (ye) (-2) 1
  --light (Light 0) $= Enabled
  
  mainLoop
  --tamX (size tx _) = tx
  --tamY (size _ ty) = ty

desplegar posX posY c angulo = do
        
        
         ce<- get c

         --lighting $= Enabled
         px <- get posX
         py <- get posY
         let x = -1 + (fromIntegral px)/ 250.0
         let y = -1 + (500.0 - fromIntegral py)/250.0
         position (Light 0) $= Vertex4 (x) (y) (-2) 1
         --light (Light 0) $= Enabled
         matrixMode$= Projection
         perspective 60 1 ce 5
         lookAt (Vertex3 0 0 2) (Vertex3 0 0 0) (Vector3 0 1 0)
         matrixMode$= Modelview 0
         loadIdentity

  
         

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
                            angulo $= a+0.01                            
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
--cubo :: GLfloat -> IO()
cubo tam = do
  materialDiffuse Back$= Color4 0 1 0 1
  superficie [(0,0,0),(tam,0,0),(tam,tam,0),(0,tam,0)] (0, 0, -1)
  materialDiffuse Front$= Color4 0 0 1 1
  superficie [(0,0,0),(0,tam,0),(0,tam,tam),(0,0,tam)] ( -1, 0, 0)
  materialDiffuse Front$= Color4 0 0 0 1
  superficie [(0,0,tam),(tam,0,tam),(tam,tam,tam),(0,tam,tam)] (0, 0, 1)
  materialDiffuse Front$= Color4 1 1 1 1
  superficie [(0,0,0),(tam,tam,0),(tam,tam,tam),(tam,0,tam)] (1, 0, 0)
  materialDiffuse Front$= Color4 1 1 0 1
  superficie [(0,0,0),(tam,0,0),(tam,0,tam),(0,0,tam)] (0, -1, 0)
  materialDiffuse Front$= Color4 1 0 1 1
  superficie [(0,tam,0),(tam,tam,0),(tam,tam,tam),(0,tam,tam)] (0, 1, 0)

superficie :: [(GLfloat,GLfloat,GLfloat)]->(GLfloat,GLfloat,GLfloat)->IO()
superficie puntos (nx, ny, nz)= do
  renderPrimitive Polygon$do
     normal (Normal3 nx ny nz)
     mapM_ (\(x,y,z) -> vertex $ Vertex3 x y z) puntos
--movRaton 
movRaton posX posY (Position  x y)= do 
        posX $= x
        posY $= y
        postRedisplay Nothing
raton  c (MouseButton LeftButton) Down _ (Position x y) = do
        ce<- get c
        c $= (ce + 0.5)
        postRedisplay Nothing
raton  c (MouseButton RightButton) Down _ (Position x y) = do
        ce<- get c
        c $= (ce - 0.5)
        postRedisplay Nothing
raton  _ _ _ _ _ = do
        --c $= (c + 0.5)
        postRedisplay Nothing
