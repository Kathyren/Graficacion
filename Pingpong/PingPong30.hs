{-# LANGUAGE FlexibleContexts #-}
import Graphics.UI.GLUT
import Data.IORef
--import PointsForRendering      
--data mesita =  Null | [(GLfloat,GLfloat,GLfloat)]

_TIEMPO=0.008
_RadioFranja=0.01
_RADIO=0.3
_RADIOBlanco=0.3
_Vel= 0.0009
_RangoZ = 0.85
_TL=(-4.4 ) ---Este valor representa los limites de la parabola
_DisPelota = 0.80
_Pendiente =0
_Rx = 0
_MaxVidas=3
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
      posBY <- sequence [newIORef (0.2), newIORef (0.1), newIORef (0.2), newIORef (0.1), newIORef (0.6), newIORef (-0.1), newIORef (0.1), newIORef (0.2)]
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
      vidas <- newIORef _MaxVidas
      m <- newIORef (_Pendiente)
      t <- newIORef (_TL)
      rx <- newIORef (_Rx)      
      puntos <- newIORef 0
      golpe <- newIORef 0
      anguloPelota <- newIORef 0
      p <- newIORef 0 ----Si obtuvo un punto
      tiempoLapso <- newIORef _TIEMPO
      vi <- newIORef _Vel
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
      idleCallback $=Just (temporizador pelotaX pelotaY pelotaZ tiempo ida altura t fuerza preX posX posY puntos rx m golpe anguloPelota tiempoLapso estado p posBX posBY modo index vi vidas)
      
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
                            --dibPaleta ((xu+0.25), (yu+0.25), _DisPelota) mi
                            dibPaleta ((xu), (yu), _DisPelota) mi
                            dibPaleta ((xu), (yu), _DisPelota+0.05) mi
                            --dibPaleta ((xu+0.25), (yu+0.25), (_DisPelota+0.005)) mi
                            generar (0,-0.3,0) 1.8 0.2 (2.5 * _RangoZ)
                          preservingMatrix $ do ---LINEAS PARA PERSPECIVA
                            rotate 350 $ Vector3 0 1 (0::GLfloat)
                            poliLinea [((-1),(-1),2),((-1),(-1),(-2)),((-1),(-1),(-2)),(2,(-1),(-2))]
                            poliLinea [((-1),(-1),(-2)),((-1),2,(-2))]
               else if (mo==2) then do---NORMAL
                          clearColor $= Color4 0.5 1 1 1
                          clear [ColorBuffer, DepthBuffer]
                          currentRasterPosition $= Vertex4 (-1.0) (0.8) 0.0 1.0
                          renderString TimesRoman24 $ show p
                          currentRasterPosition $= Vertex4 (0.90) (-0.0) 0.0 1.0
                          renderString TimesRoman24 $ show ap
                          currentRasterPosition $= Vertex4 (-0.40) (0.8) 0.0 1.0

                          preservingMatrix $ do------Pelota
                                   translate $ Vector3 (x) (y) (z::GLfloat)
                                   display 

                          --let m = -1 + (fromIntegral mi)/ 250.0
                          preservingMatrix $ do------Paleta y Mesa
                                   
                            rotate 350 $ Vector3 0 1 (0::GLfloat)
                            dibPaleta ((xu), (yu), _DisPelota) mi
                            dibPaleta ((xu), (yu), _DisPelota+0.05) mi
                            generar (0,-0.3,0) 1.8 0.2 (2.5 * _RangoZ)
                          preservingMatrix $ do ---LINEAS PARA PERSPECIVA
                            rotate 350 $ Vector3 0 1 (0::GLfloat)
                            poliLinea [((-1),(-1),2),((-1),(-1),(-2)),((-1),(-1),(-2)),(2,(-1),(-2))]
                            poliLinea [((-1),(-1),(-2)),((-1),2,(-2))]
                          if (e<0)then do
                          
                                     currentRasterPosition $= Vertex4 (-0.40) (0.8) 0.0 1.0
                                     renderString TimesRoman24 $ "GAME OVER"
                                     currentRasterPosition $= Vertex4 (-1) (0.0) 0.0 1.0
                                     renderString TimesRoman24 $ "Presione 5 para volver al menú"
                           else
                          
                                     --currentRasterPosition $= Vertex4 (1) (-1) 0.0 1.0
                                     renderString TimesRoman24 $ "☺"
               else if (mo==4) then do----CRAZY
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
                            dibPaleta ((xu), (yu), _DisPelota) mi
                            dibPaleta ((xu), (yu), _DisPelota+0.05) mi
                            generar (0,-0.3,0) 1.8 0.2 (2.4 * _RangoZ)
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
                          preservingMatrix $ do

                                  translate $ Vector3 (x) (-0.1990) ((z)::GLfloat)
                                  rotate 80 $ Vector3 1 0 (0::GLfloat)
                                  materialDiffuse Front$= Color4 0.01 0.01 0.01 1
                                  superficie (sombra (0.1)) (0,1,0)
                                  translate $ Vector3 (-x) (-y) (-(z)::GLfloat)
                          --let m = -1 + (fromIntegral mi)/ 250.0
                          preservingMatrix $ do------Paleta y Mesa
                                   
                            rotate 350 $ Vector3 0 1 (0::GLfloat)
                            dibPaleta ((xu), (yu), _DisPelota) mi
                            dibPaleta ((xu), (yu), _DisPelota+0.05) mi
                            dibBlanco (bx,by, (0 -_RangoZ-0.1)) _RADIOBlanco
                            generar (0,-0.3,0) 1.8 0.2 (2.8 * _RangoZ)

                          preservingMatrix $ do ---LINEAS PARA PERSPECIVA
                            rotate 350 $ Vector3 0 1 (0::GLfloat)
                            poliLinea [((-1),(-1),2),((-1),(-1),(-2)),((-1),(-1),(-2)),(2,(-1),(-2))]
                            poliLinea [((-1),(-1),(-2)),((-1),2,(-2))]
                          if (e<0)then do
                          
                                     currentRasterPosition $= Vertex4 (-0.40) (0.8) 0.0 1.0
                                     renderString TimesRoman24 $ "GAME OVER"
                                     currentRasterPosition $= Vertex4 (-1) (0.0) 0.0 1.0
                                     renderString TimesRoman24 $ "Presione 5 para volver al menú"
                           else
                          
                                     --currentRasterPosition $= Vertex4 (1) (-1) 0.0 1.0
                                     renderString TimesRoman24 $ "☺"      
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
  superficie (pelota m) (0, 0, 1)
  translate $ Vector3 (-px) (-py) ((-pz)::GLfloat)
  flush

pelota :: GLfloat -> [(GLfloat,GLfloat,GLfloat)]
pelota m =  [ ((_RADIO*(sin (2*pi*k/30))), (_RADIO* (cos (2*pi*k/30))), ((_RADIO*(sin (2*pi*k/30))*m))) | k <- [1..30] ]
------Blancos
dibBlanco ::(GLfloat, GLfloat, GLfloat) ->GLfloat -> DisplayCallback
     
dibBlanco (px, py, pz) r = do 
  --px<- get pXx
 -- py<- get pYx
 -- pz<- get pZx
  materialDiffuse Front$= Color4 1 1 1 1
  position (Light 1) $= Vertex4 (px) (-0.5) (-0.5) 1
  light (Light 1) $= Enabled
  --translate $ Vector3 (px) (py) ((pz)::GLfloat)
  coloreaBlanco (px, py, (pz+0.0001)) (r-_RadioFranja) (3)    
--  preservingMatrix $ do
--     translate $ Vector3 (px) (py) ((pz)::GLfloat)
--     materialDiffuse Front$= Color4 1 1 1 1
--     superficie (blanco r) (0,0,1)
--     translate $ Vector3 (-px) (-py) (-(pz)::GLfloat)
  preservingMatrix $ do

     translate $ Vector3 (px) (-0.1990) ((pz)::GLfloat)
     rotate 80 $ Vector3 1 0 (0::GLfloat)
     materialDiffuse Front$= Color4 0.01 0.01 0.01 1
     superficie (sombra (r)) (0,1,0)
     translate $ Vector3 (-px) (-py) (-(pz)::GLfloat)
  --translate $ Vector3 (-px) (-py) ((-pz)::GLfloat)
  flush

coloreaBlanco (px, py, pz) r n =
  if (n>0) then do
    preservingMatrix $ do
       translate $ Vector3 (px) (py) (((pz+0.001))::GLfloat)
       if ( (mod n 2)==0) then
           materialDiffuse Front$= Color4 1 1 1 1
       else
           materialDiffuse Front$= Color4 1 0 0 1
       superficie (blanco r) (0,0,1)
       --coloreaBlanco (px, py, ((pz)) ) (r-0.1) (n-1)    
       
       translate $ Vector3 (-px) (-py) ((-(pz))::GLfloat) 
    coloreaBlanco (px, py, ((pz+0.001))) (r-0.1) (n-1)  
       --flush 
  else 
      return ()
blanco :: GLfloat -> [(GLfloat,GLfloat,GLfloat)]
blanco r =  [ ((r*(sin (2*pi*k/20))), (r* (cos (2*pi*k/20))), 0) | k <- [1..20] ]
sombra :: GLfloat -> [(GLfloat,GLfloat,GLfloat)]
sombra r =  [ ((r*(sin (2*pi*k/30))), (r* (cos (2*pi*k/20))),0) | k <- [1..30] ]

-----Lineas de fondo

--Generar cualquier superficie 3D.
superficie :: [(GLfloat,GLfloat,GLfloat)]->(GLfloat,GLfloat,GLfloat)->IO()
superficie puntos (nx, ny, nz)= do
  renderPrimitive Polygon$do
     normal (Normal3 nx ny nz)
     mapM_ (\(x,y,z) -> vertex $ Vertex3 x y z) puntos
-------------------------Tiempos-----------------     
--temporizador:: IORef ->IORef ->IORef ->IORef ->IORef ->IORef -> IORef ->DisplayCallback
temporizador pelotaX   pelotaY pelotaZ tiempo  ida   altura  ti fuerza preX xleta yleta puntos rx pendiente golpe anguloPelota tiempoLapso estado pu xb yb modo index vi vidas= do
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
       tiL <- get tiempoLapso
       v <- get vi
       pun <- get pu----Si anotó punto
       vida <- get vidas
       mo <- get modo
       ind <- get index
       bx <- get (xb!!ind)
       by <- get (yb!!ind)
       if (e>0) then
         
            

        if t>=1.0 then do
                      if ( f > (0) && vida>0 ) then do

---------------------------CHOQUE
                          let clic = 0
                          let xu = -1 + (fromIntegral xl)/ 250.0
                          let yu = -1 + (500.0 - fromIntegral yl)/250.0
                          let az= 0
                          
                          anguloPelota$= vida




                          if ((z+ 0.1)<= (_DisPelota + 0.09) && (z+0.1)>= (_DisPelota - 0.09) && y<= (yu + _RADIO) &&  y>= (yu- _RADIO) && x<= (xu + _RADIO) &&  x>= (xu - _RADIO))    then do
                                     if (id==0 && gol<1 ) then do
                                        if ( mo/=3) then do
                                            puntos$= p + 1
                                            golpe$=1
                                            vi$= v + 0.0001
                                        else
                                            ida$= 1
                                        --fuerza$= f + 0.002

                                         
                                        ida$= 1
                                     else do
                                         ida$= id--sige viniendo



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
                                            pelotaX$= x + (v  * angX)/10
                                          
                                            if ((x>=(0.8))|| (x<=(-0.8))) then do
                                                  rx$= (-1)* angX   --------------------------para saber si lo sumo o lo resto
                                                  if (x>=(0.8)) then do
                                                      pelotaX$= x + (v  * angX)  - 0.1
                                                  else
                                                      pelotaX$= x + (v  * angX)  + 0.1
                                            else 
                                                  pelotaX$= x + (v  * angX)/10
                                            
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
-----------------------------CHOQUE CON BLANCO (SOLO PARA MODO 3)
                              else if ((z- 0.1)<= (-_RangoZ +0.01)  && y<= (by + _RADIOBlanco) &&  y>= (by- _RADIOBlanco) && x<= (bx + _RADIOBlanco) &&  x>= (bx - _RADIOBlanco) && mo==3) then do
                                     
                                     if (id==1 && gol<1) then do
                                            puntos$= p + 1
                                            fuerza$= f + 0.002
                                            vi$= v+0.0001 
                                            if ((z- 0.1)<= (-_RangoZ +0.01)  && y<= (by + _RADIOBlanco-0.05) &&  y>= (by- _RADIOBlanco-0.05) && x<= (bx + _RADIOBlanco-0.05) &&  x>= (bx - _RADIOBlanco-0.05) ) then do
                                             puntos$= p + 1
                                            else do
                                               ida$= 1
                                            golpe$=1
                                            ida$= 0
                                            index$= ind +1
                                     else do
                                         ida$= 0
                                         --golpe$=gol
                                     



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
                                            pelotaX$= x + (v  * angX)/10
                                          
                                            if ((x>=(0.8))|| (x<=(-0.8))) then do
                                                  rx$= (-1)* angX   --------------------------para saber si lo sumo o lo resto
                                                  if (x>=(0.8)) then do
                                                      pelotaX$= x + (v  * angX)  - 0.1
                                                  else
                                                      pelotaX$= x + (v  * angX)  + 0.1
                                            else 
                                                  pelotaX$= x + (v  * angX)/10
                                            
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
                                     id <- get ida
                                     g <- get golpe
                                     if (z>=(_RangoZ+0.5) && mo==2)then do
                                                      
                                                      if (vida<1) then
                                                            estado$= (-1)
                                                      else
                                                        vidas$= vida-1
                                                      pelotaX$=0
                                                      pelotaY$=0
                                                      pelotaZ$=0
                                      else do
                                          if (z>=(_RangoZ) && mo/=2) then do -----retocede
                                            ida$= 1
                                            if (mo == 3) then
                                               golpe$=0
                                            else
                                               ida$= 1
                                          else if (z<=(-(_RangoZ))) then do
                                             ida$= 0
                                             if (mo == 3) then
                                               ida$= 0
                                             else
                                               golpe$=0
                                          else
                                              ida $= id

                                          if (id == 0) then  ----avanza
                                                            pelotaZ$= z + v + az    
                                                    else
                                                           pelotaZ$= z - v  + az

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
                                   --fuerza$= f - 0.001
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
  (Char '4') -> do
    estado$=4
    modo$=4
    postRedisplay Nothing
  (Char '0') -> do
    estado$= 0 
    postRedisplay Nothing
  (Char '9') -> do
    estado$= 1 
    postRedisplay Nothing

    
--click  _ _ _ _ _ _ _ _ = return()
