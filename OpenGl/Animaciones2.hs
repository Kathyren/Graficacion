{-# LANGUAGE FlexibleContexts #-}

import Graphics.UI.GLUT
import Data.IORef

main = do
     getArgsAndInitialize
     createWindow "Graficos"
     escala <- newIORef 1.0
     angulo <- newIORef 0
     keyboardMouseCallback $= Just (teclad escala angulo)
     displayCallback $= desplegar angulo escala 
     mainLoop



desplegar angulo escala = do 
      clear [ColorBuffer]
      e <- get escala
      a <- get angulo
      scale e e (1.0::GLfloat)
      rotate a $ Vector3 0 0 (1:: GLfloat)
      triangulo (-0.2::GLfloat, -0.2::GLfloat,0::GLfloat) (0.2::GLfloat, -0.2::GLfloat,0::GLfloat) (0::GLfloat, 0.2::GLfloat,0::GLfloat)
      loadIdentity
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
                               angulo$= a -5
                               postRedisplay Nothing
     (SpecialKey KeyLeft) -> do 
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

trasladar tras bandera (SpecialKey KeyRight) Down _ _ = do
         f<- get tras
         g <- get bandera
         if f >= 0.8 then bandera $=0
         else if f<=0 -0.8 then bandera $=1
         else bandera $=g
         if g==1 then tras $= f - 0.1
         else tras $=f - 0.1
         postRedisplay Nothing
