{-# LANGUAGE FlexibleContexts #-}
import Graphics.UI.GLUT
import Data.IORef
import PointsForRendering      
--data mesita =  Null | [(GLfloat,GLfloat,GLfloat)]

_TIEMPO=0.001

main = do
      getArgsAndInitialize
      initialDisplayMode $= [WithDepthBuffer, DoubleBuffered]
      createWindow "Graficos"
      depthFunc $= Just Less
      windowSize $= Size 500 500
      windowPosition $= Position 300 100
      tiempo <- newIORef 0.0
      angulo <- newIORef 0.0
      displayCallback $= desplegar
      --displayCallback $= modela angulo
      perspective 60 1 1 5
      lookAt (Vertex3 0 1.5 2) (Vertex3 0 0 0) (Vector3 0 1 0)
      matrixMode$= Modelview 0
      lighting $= Enabled
      position (Light 0) $= Vertex4 (1.5) (1.5) (2) 1
      light (Light 0) $= Enabled
      lightModelAmbient  $= Color4 0.5 0.5 0.5 1 
      diffuse (Light 0)  $= Color4 1 1 1 1
      idleCallback $= Just (temporizador angulo tiempo)
      mainLoop

--mesa = [((-0.8),0,0.8), (0.8,0,0.8), (0.8,0,(-0.8)), ((-0.8),0,(-0.8))]
--coordenada= (0.8, 0, 0.8)
mesa :: [(GLfloat,GLfloat,GLfloat)]
mesa =  [(-0.9,-0.5,0.9),(0.9,-0.5,0.8),(0.7,0.7,-0.8),(-0.7,0.7,-0.8)]

desplegar = do 
               --clear [ColorBuffer]
               clear [ColorBuffer, DepthBuffer]
               preservingMatrix $ do
                --rotate 180 $ Vector3 1 0 (1::GLfloat)
                rotate 350 $ Vector3 0 1 (0::GLfloat)
                generar (0,-0.3,0) 1.8 1 1.8
               swapBuffers
               
modela angulo= do 
               clear [ColorBuffer]
               clear [ColorBuffer, DepthBuffer]
               a <- get angulo 
               if (a<160)then  putStrLn "-------------" else putStrLn "00000000000"
               preservingMatrix $ do
                rotate a $ Vector3 1 0 (1::GLfloat)
                generar (0,-0.3,0) 1.8 0.2 1.8
               swapBuffers
               
fondo = do
          renderPrimitive Polygon $ 
            mapM_ (\(x,y,z) -> vertex $ Vertex3 x y z) mesa
--pelota (x,y,z)=


--cuadro:: (GLfloat,GLfloat,GLfloat) -> IO()
--cuadro (x, y, z) =  renderPrimitive Polygon $ do
--            mapM_ (\(x,y,z) -> vertex $ Vertex3 x y z) mesa


----------------Graficacion de elementos--------------


--GenerarCaras
generarPrisma (x,y,z) w h l = do
    --cara A (base)
    materialDiffuse Front$= Color4 0 0 1 1
    superficie [((x-(w/2)),(y-(0)),(z-(l/2))),        ((x-(w/2)),(y+(0)),(z+(l/2))),         ((x+(w/2)),(y+(0)),(z+(l/2))),         ((x+(w/2)),(y-(0)),(z-(l/2)))] (0, 0, -1)
    --Cara B (frontal)
    materialDiffuse Front$= Color4 0 1 0 1
    superficie [((x-(w/2)),(y-(0)),(z-(l/2))),        ((x+(w/2)),(y-(0)),(z-(l/2))),         ((x),(h),(z))] (-1,0, 0)
    --Cara C (trasera)
    materialDiffuse Front$= Color4 0 1 1 1
    superficie [((x-(w/2)),(y-(0)),(z-(l/2))),         ((x-(w/2)),(y+(0)),(z+(l/2))),        ((x),(h),(z))] (0, 0, 1)
    --Cara D (lateral izq)
    materialDiffuse Front$= Color4 1 0 0 1
    superficie [((x-(w/2)),(y+(0)),(z+(l/2))),          ((x+(w/2)),(y+(0)),(z+(l/2))),        ((x),(h),(z))] (1,0, 0)
    --Cara E (Arriba)
    materialDiffuse Front$= Color4 1 0 1 1
    superficie [ ((x+(w/2)),(y+(0)),(z+(l/2))),         ((x+(w/2)),(y-(0)),(z-(l/2))),        ((x),(h),(z))] (0,1,0)
    
--Generar cualquier superficie.
superficie :: [(GLfloat,GLfloat,GLfloat)]->(GLfloat,GLfloat,GLfloat)->IO()
superficie puntos (nx, ny, nz)= do
  renderPrimitive Polygon$do
     normal (Normal3 nx ny nz)
     mapM_ (\(x,y,z) -> vertex $ Vertex3 x y z) puntos
-------------------------Tiempos-----------------     
temporizador angulo tiempo = do
        a <- get angulo
        t <- get tiempo
        if t>=1.0 then do
                            tiempo $= t+_TIEMPO
                            angulo $= a+0.005                            
                            postRedisplay Nothing
                      else do
                            tiempo $= t+_TIEMPO
                            return ()