{-# LANGUAGE FlexibleContexts #-}
import Graphics.UI.GLUT
import Data.IORef
--import PointsForRendering      
--data mesita =  Null | [(GLfloat,GLfloat,GLfloat)]

_TIEMPO=0.001
_RADIO=0.3
_Vel= 0.0009
_RangoZ = 0.85
_TL=(-4.02 ) ---Este valor representa los limites de la parabola
_DisPelota = 0.85
_Pendiente =0
_Rx = 0

main = do
      getArgsAndInitialize
      initialDisplayMode $= [WithDepthBuffer, DoubleBuffered]
      createWindow "Graficos"
      estado <-newIORef 0
      modo <- newIORef 0
      depthFunc $= Just Less
      windowSize $= Size 500 500
      windowPosition $= Position 300 100
      clearColor $= Color4 0.5 0.5 1 1
      posBX <- sequence [newIORef (0.2), newIORef (0.1), newIORef (0.2), newIORef (0.1), newIORef (-0.6), newIORef (-0.1), newIORef (-0.5), newIORef (0.5)]
      posBY <- sequence [newIORef (0.2), newIORef (0.1), newIORef (0.2), newIORef (0.1), newIORef (-0.6), newIORef (-0.1), newIORef (-0.5), newIORef (0.5)]
      index <- newIORef 0
      posX <- newIORef 0
      posY <- newIORef 0
      tiempo <- newIORef 0.0
      angulo <- newIORef 0.0
      pelotaX <- newIORef 0
      preX <- newIORef 0
      pelotaY <- newIORef 0
      pelotaZ <- newIORef 0.0  
      ida <- newIORef 0.0
      altura <- newIORef 0.0
      fuerza <- newIORef 1
      m <- newIORef (_Pendiente)
      t <- newIORef (_TL)
      rx <- newIORef (_Rx)      
      puntos <- newIORef 0
      golpe <- newIORef 0
      anguloPelota <- newIORef 0
      p <- newIORef 0 ----Si obtuvo un punto
      tiempoLapso <- newIORef _TIEMPO
      loadIdentity
      perspective 60 1 (0.5) 5
      lookAt (Vertex3 0 1.0 2.2) (Vertex3 0 0 0) (Vector3 0 1 0)
      matrixMode$= Modelview 0
      lighting $= Enabled
      position (Light 0) $= Vertex4 (1) (1) (2) 1
      light (Light 0) $= Enabled
      --lightModelAmbient  $= Color4 0.5 0.5 0.5 1 
      lightModelAmbient  $= Color4 1 0.5 1 1 
      diffuse (Light 0)  $= Color4 1 1 1 1
      keyboardMouseCallback $= Just (click m rx estado modo)

      displayCallback $= desplegar pelotaX pelotaY pelotaZ (posX, posY) m puntos anguloPelota estado modo posBX posBY index
      keyboardMouseCallback $= Just (click m rx estado modo)

      ind <- get index
      setMaterial
      --idleCallback $= Just (animacion p tiempo)
      idleCallback $=Just (temporizador pelotaX pelotaY pelotaZ tiempo ida altura t fuerza preX posX posY puntos rx m golpe anguloPelota tiempoLapso estado p (posBY!!ind) (posBY!!ind))
      
      passiveMotionCallback $= Just (movRaton posX posY)
      mainLoop

elEstado estado|estado==1 =True
               |otherwise =False

--mesa = [((-0.8),0,0.8), (0.8,0,0.8), (0.8,0,(-0.8)), ((-0.8),0,(-0.8))]
--coordenada= (0.8, 0, 0.8)
mesa :: [(GLfloat,GLfloat,GLfloat)]
mesa =  [(-0.9,-0.5,0.9),(0.9,-0.5,0.8),(0.7,0.7,-0.8),(-0.7,0.7,-0.8)]
puntoMas x y  = do

      clearColor $= Color4 0.0 0.0 0 1
      currentColor $= Color4 0 0 0 1
      clear [ColorBuffer, DepthBuffer]
       
      currentRasterPosition $= Vertex4 (x) (y) 0.0 1.0
      renderString TimesRoman24 $ "+1"
desplegar x1 y1 z1 (xp, yp) pendiente puntos anguloPelota estado modo posBX posBY index = do 
               --clear [ColorBuffer]
               x<- get x1
               y<- get y1
               z<- get z1
               x2<- get xp
               y2<- get yp
               mi <- get pendiente
               p <- get puntos
               ap <- get anguloPelota
               e <- get estado
               mo <- get modo
               
               ind <- get index
               bx <- get (posBX !! ind)
               by <- get (posBY !! ind)
               let xu = -1 + (fromIntegral x2)/ 250.0
               let yu = -1 + (500.0 - fromIntegral y2)/250.0
               if (mo==0) then do 
                          clearColor $= Color4 0.0 0.0 0 1
                          currentColor $= Color4 0 0 0 1
                          clear [ColorBuffer, DepthBuffer]
                           
                          currentRasterPosition $= Vertex4 (-0.40) (0.8) 0.0 1.0
                          renderString TimesRoman24 $ "PINGPOINT"
                          currentRasterPosition $= Vertex4 (-0.7) (0.50) 0.0 1.0
                          renderString TimesRoman24 $ "Seleccione un modo para jugar"
                          currentRasterPosition $= Vertex4 (-0.80) (-0.0) 0.0 1.0
                          renderString TimesRoman24 $ "1.- Modo Zen. \n 2.- Normal."
                          currentRasterPosition $= Vertex4 (-1.2) (-0.2) 0.0 1.0
                          renderString TimesRoman24 $ "3.-Esquiva obstaculo. \n 4.- CrazyMode!!!"
               else if (mo==1) then do
                          clearColor $= Color4 0.5 0.5 1 1
                          clear [ColorBuffer, DepthBuffer]
                          currentRasterPosition $= Vertex4 (-1.0) (0.8) 0.0 1.0
                          renderString TimesRoman24 $ show p
                          currentRasterPosition $= Vertex4 (0.90) (-0.0) 0.0 1.0
                          renderString TimesRoman24 $ show ap
                          
                          preservingMatrix $ do------Pelota
                                   translate $ Vector3 (x) (y) (z::GLfloat)
                                   display 

                          --let m = -1 + (fromIntegral mi)/ 250.0
                          preservingMatrix $ do------Paleta y Mesa
                                   
                            rotate 350 $ Vector3 0 1 (0::GLfloat)
                            dibPaleta ((xu+0.25), (yu+0.25), _DisPelota) mi
                            dibPaleta ((xu+0.25), (yu+0.25), (_DisPelota+0.1)) mi
                            generar (0,-0.3,0) 1.8 0.2 2
                          preservingMatrix $ do ---LINEAS PARA PERSPECIVA
                            rotate 350 $ Vector3 0 1 (0::GLfloat)
                            poliLinea [((-1),(-1),2),((-1),(-1),(-2)),((-1),(-1),(-2)),(2,(-1),(-2))]
                            poliLinea [((-1),(-1),(-2)),((-1),2,(-2))]
               else if (mo==2) then do
                          clearColor $= Color4 0.5 0.5 1 1
                          clear [ColorBuffer, DepthBuffer]
                          currentRasterPosition $= Vertex4 (-1.0) (0.8) 0.0 1.0
                          renderString TimesRoman24 $ show p
                          currentRasterPosition $= Vertex4 (0.90) (-0.0) 0.0 1.0
                          renderString TimesRoman24 $ show ap
                          
                          preservingMatrix $ do------Pelota
                                   rotate 150 $ Vector3 0 1 (0::GLfloat)
                                   translate $ Vector3 (x) (y) (z::GLfloat)
                                   display 
                          
                          --let m = -1 + (fromIntegral mi)/ 250.0
                          preservingMatrix $ do------Paleta y Mesa
                                   
                            rotate 100 $ Vector3 0 1 (0::GLfloat)
                            dibPaleta ((xu+0.25), (yu+0.25), _DisPelota) mi
                            dibPaleta ((xu+0.25), (yu+0.25), (_DisPelota+0.1)) mi
                            generar (0,-0.3,0) 1.8 0.2 2
                          preservingMatrix $ do ---LINEAS PARA PERSPECIVA
                            rotate 100 $ Vector3 0 1 (0::GLfloat)
                            poliLinea [((-1),(-1),2),((-1),(-1),(-2)),((-1),(-1),(-2)),(2,(-1),(-2))]
                            poliLinea [((-1),(-1),(-2)),((-1),2,(-2))]
               else do --Blancos
                          clearColor $= Color4 0.5 0.5 1 1
                          clear [ColorBuffer, DepthBuffer]
                          currentRasterPosition $= Vertex4 (-1.0) (0.8) 0.0 1.0
                          renderString TimesRoman24 $ show p
                          currentRasterPosition $= Vertex4 (0.90) (-0.0) 0.0 1.0
                          renderString TimesRoman24 $ show ap
                          preservingMatrix $ do------Pelota
                                   --rotate 150 $ Vector3 0 1 (0::GLfloat)
                                   translate $ Vector3 (x) (y) (z::GLfloat)
                                   display 
                          
                          --let m = -1 + (fromIntegral mi)/ 250.0
                          preservingMatrix $ do------Paleta y Mesa
                                   
                            rotate 350 $ Vector3 0 1 (0::GLfloat)
                            dibPaleta ((xu+0.25), (yu+0.25), _DisPelota) mi
                            dibPaleta ((xu+0.25), (yu+0.25), (_DisPelota+0.1)) mi
                            dibBlanco (bx,by, (0 -_RangoZ-0.1)) 0.5
                            generar (0,-0.3,0) 1.8 0.2 2

                          preservingMatrix $ do ---LINEAS PARA PERSPECIVA
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
dibPaleta ::(GLfloat, GLfloat, GLfloat) ->GLfloat -> DisplayCallback
     
dibPaleta (px, py, pz) m= do 
  --px<- get pXx
 -- py<- get pYx
 -- pz<- get pZx
  materialDiffuse Front$= Color4 1 0 1 1
  translate $ Vector3 (px) (py) ((pz)::GLfloat)
  renderPrimitive Polygon $
      mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z) (pelota m)
  translate $ Vector3 (-px) (-py) ((-pz)::GLfloat)
  flush

pelota :: GLfloat -> [(GLfloat,GLfloat,GLfloat)]
pelota m =  [ ((_RADIO*(sin (2*pi*k/12))), (_RADIO* (cos (2*pi*k/12))), ((_RADIO*(sin (2*pi*k/12))*m))) | k <- [1..12] ]
------Blancos
dibBlanco ::(GLfloat, GLfloat, GLfloat) ->GLfloat -> DisplayCallback
     
dibBlanco (px, py, pz) r= do 
  --px<- get pXx
 -- py<- get pYx
 -- pz<- get pZx
  materialDiffuse Front$= Color4 1 1 1 1
  
  --renderPrimitive Polygon $
  --position (Light 1) $= Vertex4 (-0.5) (-0.5) (-0.5) 1
  position (Light 1) $= Vertex4 (px) (-0.5) (-0.5) 1
  light (Light 1) $= Enabled
  translate $ Vector3 (px) (py) ((pz)::GLfloat)
  materialDiffuse Front$= Color4 1 1 1 1
  superficie (blanco r) (0, 0, (1))
  materialDiffuse Front$= Color4 1 0 0 1
  superficie (blanco (r-0.1)) (0, 0, (1))      
  translate $ Vector3 (-px) (-py) ((-pz)::GLfloat)
  flush

blanco :: GLfloat -> [(GLfloat,GLfloat,GLfloat)]
blanco r =  [ ((r*(sin (2*pi*k/20))), (r* (cos (2*pi*k/20))), 0) | k <- [1..20] ]

-----Lineas de fondo

--Generar cualquier superficie 3D.
superficie :: [(GLfloat,GLfloat,GLfloat)]->(GLfloat,GLfloat,GLfloat)->IO()
superficie puntos (nx, ny, nz)= do
  renderPrimitive Polygon$do
     normal (Normal3 nx ny nz)
     mapM_ (\(x,y,z) -> vertex $ Vertex3 x y z) puntos
-------------------------Tiempos-----------------     
--temporizador:: IORef ->IORef ->IORef ->IORef ->IORef ->IORef -> IORef ->DisplayCallback
temporizador pelotaX   pelotaY pelotaZ tiempo  ida   altura  ti fuerza preX xleta yleta puntos rx pendiente golpe anguloPelota tl estado pu xb yb= do
       z <- get pelotaZ
       y <- get pelotaY 
       x <- get pelotaX     
       t <- get tiempo
       id <- get ida
       a <- get altura
       tiem <- get ti
       f <- get fuerza
       prx <- get preX
       yl <- get yleta
       xl <- get xleta
       p <-get puntos
       angX <- get rx
       m <- get pendiente
       gol <- get golpe
       anPel <- get anguloPelota
       e <- get estado
       tiL <- get tl
       pun <- get pu----Si anotó punto
       if (e>0) then
         
            

        if t>=1.0 then do
                      if ( f > (0) ) then do

---------------------------CHOQUE
                          let clic = 0
                          let xu = -1 + (fromIntegral xl)/ 250.0
                          let yu = -1 + (500.0 - fromIntegral yl)/250.0
                          let az= 0
                          if ((z+ 0.1)<= (_DisPelota + 0.09) && (z+0.1)>= (_DisPelota - 0.09) && y<= (yu + _RADIO) &&  y>= (yu- _RADIO) && x<= (xu + _RADIO) &&  x>= (xu - _RADIO) ) then do
                                     if (id==0 && gol<1) then do
                                          
                                            puntos$= p + 1

                                            golpe$=1
                                            ida$= 1
                                     else do
                                         ida$= 0



                                     if (angX==0) then
                                         pelotaX$=x
                                     else if (angX == 1) then  do
                                          --clic $= True
                                          pelotaX$= (x - 0.0008)  
                                     else if angX == 2 then  do
                                          --clic $= True
                                          pelotaX$= (x + 0.0008) 
                                            
                                     else do
                                    --ida $= id  do
           
                                            rx $= atan((z-_DisPelota-m)/(1+ ((x-xu)*m)))+3
                                            anguloPelota$= atan((z-_DisPelota)/(x-xu)) +3
                                            pelotaX$= x + (_Vel  * angX)/10
                                          
                                            if ((x>=(0.8))|| (x<=(-0.8))) then do
                                                  rx$= (-1)* angX   --------------------------para saber si lo sumo o lo resto
                                                  if (x>=(0.8)) then do
                                                      pelotaX$= x + (_Vel  * angX)  - 0.1
                                                  else
                                                      pelotaX$= x + (_Vel  * angX)  + 0.1
                                            else 
                                                  pelotaX$= x + (_Vel  * angX)/10
                                            
                                     if (pun>0) then
                                          puntoMas 1 1 
                                     else
                                          puntoMas (-1) (-1)

------------------------------CAMBIO EN X
--                                     if (angX == 1) then 
--                                          pelotaX$= (x - 0.0008)  
--                                    else if angX == 2 then
--                                       pelotaX$= (x + 0.0008) 
--                                           else pelotaX$= x 
                              else if ((z+ 0.1)<= (_DisPelota + 0.09) && (z+0.1)>= (_DisPelota - 0.09) && y<= (yu + _RADIO) &&  y>= (yu- _RADIO) && x<= (xu + _RADIO) &&  x>= (xu - _RADIO) ) then do
                                     if (id==0 && gol<1) then do
                                          
                                            puntos$= p + 1

                                            golpe$=1
                                            ida$= 1
                                     else do
                                         ida$= 0



                                     if (angX==0) then
                                         pelotaX$=x
                                     else if (angX == 1) then  do
                                          --clic $= True
                                          pelotaX$= (x - 0.0008)  
                                     else if angX == 2 then  do
                                          --clic $= True
                                          pelotaX$= (x + 0.0008) 
                                            
                                     else do
                                    --ida $= id  do
           
                                            rx $= atan((z-_DisPelota-m)/(1+ ((x-xu)*m)))+3
                                            anguloPelota$= atan((z-_DisPelota)/(x-xu)) +3
                                            pelotaX$= x + (_Vel  * angX)/10
                                          
                                            if ((x>=(0.8))|| (x<=(-0.8))) then do
                                                  rx$= (-1)* angX   --------------------------para saber si lo sumo o lo resto
                                                  if (x>=(0.8)) then do
                                                      pelotaX$= x + (_Vel  * angX)  - 0.1
                                                  else
                                                      pelotaX$= x + (_Vel  * angX)  + 0.1
                                            else 
                                                  pelotaX$= x + (_Vel  * angX)/10
                                            
                                     if (pun>0) then
                                          puntoMas 1 1 
                                     else
                                          puntoMas (-1) (-1)

------------------------------CAMBIO EN X
--                                     if (angX == 1) then 
--                                          pelotaX$= (x - 0.0008)  
--                                    else if angX == 2 then
--                                       pelotaX$= (x + 0.0008) 
--                                           else pelotaX$= x 
                              else do
                                    --ida $= id  
                        ---------CAMBIO EN Z
                                    
                                    if (z>=(_RangoZ)) then -----retocede
                                            ida$= 1
                                    else if (z<=(-(_RangoZ))) then do
                                             ida$= 0
                                             golpe$=0
                                          else
                                              ida $= id

                          if (id == 0) then  ----avanza
                                            pelotaZ$= z + _Vel + az    
                                    else
                                           pelotaZ$= z - _Vel  + az

                          --if (a == 0) then
                          
--------------------------------CAMBIO EN Y
                         -- if (y<=0) then
                          if (tiem >= (-1 *_TL) )then do
                              
                                  ti$=  _TL
                                  --ti$= sqrt (18 * f)
                          else do
                                  ti$= tiem + 0.008


                          if (y>=(1)) then do
                                  altura$= 1
                                  pelotaY$= f - (1/18)*(tiem*tiem)
                          else if (y<(0)) then do
                                   altura$= 0
                                   fuerza$= f - 0.001
                                   ti$= -sqrt (18 * f) + 0.005
                                   
                                   pelotaY$= (0.01)
                                   --ti$= tiem+0
                          
                                else do
                                   pelotaY$= f - (1/18)*(tiem*tiem)

                          --pelotaY$= f - (1/18)*(tiem*tiem)
--------------------------Mov X
                          
                             
                          
 
      
                                                    
                          postRedisplay Nothing
                      else
                        return()
                     else do
                            tiempo $= t +tiL
                            return ()
       else 
          return()
animacion pu tiempo = do
       p <- get pu
       t<- get tiempo
       if (p>0) then
         
            

          if t>=1.0 then do
                            postRedisplay Nothing
          else do
                            tiempo $= t +_TIEMPO
                            return ()
       else 
          return()

--------------------------------MOUSE MOVE------------------------
--movRaton :: IORef -> IORef -> _ -> DisplayCallback
movRaton posX posY (Position  x y)= do 
        posX $= x
        posY $= y
        
        postRedisplay Nothing

click  m rx estado modo(MouseButton LeftButton) Down _ _ = do
        m$= 0.3
        rx$= 1
        postRedisplay Nothing
click  m rx estado modo (MouseButton RightButton) Down _ _ = do
        m$= -0.3
        rx$= 2
        postRedisplay Nothing
click  m rx estado modo _ Up _ _ = do
        m$= 0
        --rx$= 0
        postRedisplay Nothing
click  m rx estado modo tecla Down _ _= case tecla of
  (Char '1') -> do 
    estado$=1
    modo$=1
    postRedisplay Nothing
  (Char '2') -> do
    estado$=2
    modo$=2
    postRedisplay Nothing
  (Char '3') -> do
    estado$=3
    modo$=3
    postRedisplay Nothing
  (Char '0') -> do
    estado$= 0 
    postRedisplay Nothing
  (Char '9') -> do
    estado$= 1 
    postRedisplay Nothing

    
--click  _ _ _ _ _ _ _ _ = return()
