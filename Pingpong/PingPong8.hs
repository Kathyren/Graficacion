{-# LANGUAGE FlexibleContexts #-}
import Graphics.UI.GLUT
import Data.IORef
--import PointsForRendering      
--data mesita =  Null | [(GLfloat,GLfloat,GLfloat)]

_TIEMPO=0.001
_RADIO=0.1
_Vel= 0.0009
_RangoZ = 0.8
_TL=(-4.16 ) ---Este valor representa los limites de la parabola

main = do
      getArgsAndInitialize
      initialDisplayMode $= [WithDepthBuffer, DoubleBuffered]
      createWindow "Graficos"
      depthFunc $= Just Less
      windowSize $= Size 500 500
      windowPosition $= Position 300 100
      clearColor $= Color4 0.5 0.5 1 1
      posX <- newIORef 0
      posY <- newIORef 0
      tiempo <- newIORef 0.0
      angulo <- newIORef 0.0
      pelotaX <- newIORef 0
      pelotaY <- newIORef 0
      pelotaZ <- newIORef 0.0  
      ida <- newIORef 0.0
      altura <- newIORef 0.0
      t <- newIORef (_TL)
      --displayCallback $= modela angulo
      loadIdentity
      perspective 60 1 (1) 5
      lookAt (Vertex3 0 1.0 2.2) (Vertex3 0 0 0) (Vector3 0 1 0)
      matrixMode$= Modelview 0
      lighting $= Enabled
      --position (Light 0) $= Vertex4 (1.5) (1.5) (2) 1
      position (Light 0) $= Vertex4 (1) (1) (2) 1
      --position (Light 1) $= Vertex4 (0) (0) (-2) 1
      light (Light 0) $= Enabled
      --light (Light 1) $= Enabled
      lightModelAmbient  $= Color4 0.5 0.5 0.5 1 
      --lightModelAmbient  $= Color4 1 1 1 1
      diffuse (Light 0)  $= Color4 1 1 1 1
      --diffuse (Light 1)  $= Color4 1 1 1 1
      --corrida pelotaX pelotaY pelotaZ temporizador angulo tiempo
      displayCallback $= desplegar pelotaX pelotaY pelotaZ (posX, posY)
      --passiveMotionCallback $= Just (movRaton posX posY)
      --keyboardMouseCallback $= Just (paleta posX posY)
      --displayCallback $= display
      --setProjection
     -- setLights
      setMaterial
      idleCallback $= Just (temporizador pelotaX pelotaY pelotaZ tiempo ida altura t)
      mainLoop

--corrida pelotaX pelotaY pelotaZ temporizador angulo tiempo =

      --Loop

--mesa = [((-0.8),0,0.8), (0.8,0,0.8), (0.8,0,(-0.8)), ((-0.8),0,(-0.8))]
--coordenada= (0.8, 0, 0.8)
mesa :: [(GLfloat,GLfloat,GLfloat)]
mesa =  [(-0.9,-0.5,0.9),(0.9,-0.5,0.8),(0.7,0.7,-0.8),(-0.7,0.7,-0.8)]

desplegar x1 y1 z1 (xp, yp)= do 
               --clear [ColorBuffer]
               x<- get x1
               y<- get y1
               z<- get z1
               x2<- get xp
               y2<- get yp
               clear [ColorBuffer, DepthBuffer]
               currentRasterPosition $= Vertex4 (-1.0) (0) 0.0 1.0
               renderString TimesRoman24 $ "Bienvenidos al juego :)"

               preservingMatrix $ do------Pelota
                        translate $ Vector3 (x) (y) (z::GLfloat)
                        display
               preservingMatrix $ do------Paleta
                        dibPaleta (x2, y2, 0)
               preservingMatrix $ do--MESA
                 --rotate 180 $ Vector3 1 0 (1::GLfloat)
                 rotate 350 $ Vector3 0 1 (0::GLfloat)
                 --generar (0,-0.3,0) 1.8 0.2 2
                 generar (0,-0.3,0) 1.8 0.2 2
               --preservingMatrix $ do--PELOTA
                 --rotate 350 $ Vector3 1 0 (1::GLfloat)
                -- dibPaleta (x, y, z)
               preservingMatrix $ do ---LINEAS PARA PERSPECIVA
                 --rotate 180 $ Vector3 1 0 (1::GLfloat)
                 rotate 350 $ Vector3 0 1 (0::GLfloat)
                 poliLinea [((-1),(-1),2),((-1),(-1),(-2)),((-1),(-1),(-2)),(2,(-1),(-2))]
                 poliLinea [((-1),(-1),(-2)),((-1),2,(-2))]
               swapBuffers
               
modela angulo= do 
               clear [ColorBuffer]
               clear [ColorBuffer, DepthBuffer]
               a <- get angulo 
               --if (a<160)then  putStrLn "-------------" else putStrLn "00000000000"
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



------------------ESFERA-------------O
setProjection :: IO ()
setProjection = do
  matrixMode $= Projection
  ortho (-1) 1 (-1) 1 0 (-1)
 
grey1,grey9,red,white :: Color4 GLfloat
grey1 = Color4 0.1 0.1 0.1 1
grey9 = Color4 0.9 0.9 0.9 1
red   = Color4 0   1   0.2   1
white = Color4 1   1   1   1
 
setLights :: IO ()
setLights = do
  let l = Light 0
  ambient  l $= grey1
  diffuse  l $= white
  specular l $= white
  position l $= Vertex4 (-4) 4 3 (0 :: GLfloat)
  light    l $= Enabled
  lighting   $= Enabled
 
setMaterial :: IO ()
setMaterial = do
  materialAmbient   Front $= grey1
  materialDiffuse   Front $= red
  materialSpecular  Front $= grey9
  materialShininess Front $= (32 :: GLfloat)
 
--display :: IO()
display  = do
  --clear [ColorBuffer]
  renderObject Solid $ Sphere' 0.1 64 64
 -- swapBuffers
---------Lineas

poliLinea::[(GLfloat,GLfloat,GLfloat)] ->DisplayCallback
poliLinea puntos = do 
           --lear [ColorBuffer]
               renderPrimitive Lines $ do
                   mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z) puntos
               flush

--GenerarCaras
generar (x,y,z) w h l = do
    --cara A (frontal)
    materialDiffuse Front$= Color4 0 0 1 1
    superficie [((x-(w/2)),(y-(h/2)),(z-(l/2))),        ((x-(w/2)),(y+(h/2)),(z-(l/2))),         ((x+(w/2)),(y+(h/2)),(z-(l/2))),         ((x+(w/2)),(y-(h/2)),(z-(l/2)))] (0, 0, -1)
    --Cara B (lateral der)
    materialDiffuse Front$= Color4 0 0 0 1
    superficie [((x-(w/2)),(y-(h/2)),(z-(l/2))),        ((x-(w/2)),(y+(h/2)),(z-(l/2))),         ((x-(w/2)),(y+(h/2)),(z+(l/2))),          ((x-(w/2)),(y-(h/2)),(z+(l/2)))] (-1,0, 0)
    --Cara C (trasera)
    materialDiffuse Front$= Color4 0 1 1 1
    superficie [((x-(w/2)),(y-(h/2)),(z+(l/2))),         ((x-(w/2)),(y+(h/2)),(z+(l/2))),         ((x+(w/2)),(y+(h/2)),(z+(l/2))),         ((x+(w/2)),(y-(h/2)),(z+(l/2)))] (0, 0, 1)
    --Cara D (lateral izq)
    materialDiffuse Front$= Color4 0.1 0.1 0.1 1
    superficie [((x+(w/2)),(y-(h/2)),(z-(l/2))),         ((x+(w/2)),(y+(h/2)),(z-(l/2))),         ((x+(w/2)),(y+(h/2)),(z+(l/2))),         ((x+(w/2)),(y-(h/2)),(z+(l/2)))] (1,0, 0)
    --Cara E (Arriba)
    materialDiffuse Front$= Color4 0 0.8 0.3 1
    superficie [((x-(w/2)),(y+(h/2)),(z-(l/2))),         ((x-(w/2)),(y+(h/2)),(z+(l/2))),         ((x+(w/2)),(y+(h/2)),(z+(l/2))),         ((x+(w/2)),(y+(h/2)),(z-(l/2)))] (0,1,0)
    --Cara F (Abajo)
    materialDiffuse Front$= Color4 0.5 1 0.5 1
    superficie [((x-(w/2)),(y-(h/2)),(z-(l/2))),          ((x-(w/2)),(y-(h/2)),(z+(l/2))),        ((x+(w/2)),(y-(h/2)),(z+(l/2))),          ((x+(w/2)),(y-(h/2)),(z-(l/2)))] (0,-1,0)

---Del circulo
--dibPelota ::(IORef,IORef,IORef)-> DisplayCallback----- dibPaleta (pXx, pYx, pZx)= do 
dibPaleta ::(GLfloat, GLfloat, GLfloat)-> DisplayCallback
     
dibPaleta (px, py, pz)= do 
  --px<- get pXx
 -- py<- get pYx
 -- pz<- get pZx
  materialDiffuse Front$= Color4 1 0 1 1
  translate $ Vector3 (px) (py) ((pz)::GLfloat)
  renderPrimitive Polygon $
      mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z) pelota
  translate $ Vector3 (-px) (-py) ((-pz)::GLfloat)
  flush

pelota :: [(GLfloat,GLfloat,GLfloat)]
pelota = [ ((_RADIO*(sin (2*pi*k/12))), (_RADIO* (cos (2*pi*k/12))), (0)) | k <- [1..12] ]
-----Lineas de fondo

--Generar cualquier superficie 3D.
superficie :: [(GLfloat,GLfloat,GLfloat)]->(GLfloat,GLfloat,GLfloat)->IO()
superficie puntos (nx, ny, nz)= do
  renderPrimitive Polygon$do
     normal (Normal3 nx ny nz)
     mapM_ (\(x,y,z) -> vertex $ Vertex3 x y z) puntos
-------------------------Tiempos-----------------     
--temporizador:: IORef ->IORef ->IORef ->IORef ->IORef ->IORef -> IORef ->DisplayCallback
temporizador pelotaX   pelotaY pelotaZ tiempo  ida   altura  ti = do
        z <- get pelotaZ
        y <- get pelotaY      
        t <- get tiempo
        id <- get ida
        a <- get altura
        tiem <- get ti
        if t>=1.0 then do
                          if (id == 0) then
                                  pelotaZ$= z + _Vel    
                          else
                                 pelotaZ$= z - _Vel  
                          if (z>=(_RangoZ)) then
                                  ida$= 1
                          else if (z<=(-(_RangoZ))) then
                                   ida$= 0
                                else
                                    ida $= id

                          --if (a == 0) then
                          pelotaY$= 1 - (1/18)*(tiem*tiem)
                          --pelotaY$= y + (-1*(tiem*tiem)+1)
                            --pelotaY$= y + _Vel
                          --else
                           -- pelotaY$= ((1/18)*(tiem*tiem))
                            --pelotaY$= y - _Vel
                          if (tiem >= (-1 *_TL) )then do
                                  altura$= 1
                                  ti$=  _TL
                          else 
                                  ti$= tiem + 0.008
                          postRedisplay Nothing
                     else do
                            tiempo $= t +_TIEMPO
                            return ()
--------------------------------MOUSE MOVE------------------------
--movRaton :: IORef -> IORef -> (_, _, _) -> DisplayCallback
movRaton posX posY (Position  x y)= do 
        posX $= x
        posY $= y
        
        postRedisplay Nothing