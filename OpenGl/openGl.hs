{-# LANGUAGE FlexibleContexts #-}
import Graphics.UI.GLUT
--main :: IO ()
_TIEMPO = 0.0001
main = do   getArgsAndInitialize
            createWindow "Hola Mundo"
            displayCallback $= desplegar
            mainLoop

despliega = do clear [ColorBuffer]
               renderPrimitive Points $ do
                  vertex $ Vertex3 0 0 (0::GLfloat)
                  vertex $ Vertex3 0.5 0 (0::GLfloat)
               flush

myPoints :: [(GLfloat,GLfloat,GLfloat)]
myPoints = [(-0.75, -0.75,0), (0.75,-0.75,0), (0.75,0.75,0), (0.0,-0.60,0),(0.60,0.75,0),(0.60,0.75,0),  (-0.75,-0.6,0)]
--myPoints = [(-0.5,-0.5,0),(-0.5,-0.25,0),(-0.25,-0.5,0),(-0.25,-0.25,0)]
--myPoints =  [(-0.5,-0.5,0) ,(-0.25,-0.5,0) ,(-0.25,-0.75,0),(-0.25,-0.5,0) ,(-0.25,-0.75,0),(-0.25,-0.5,0) ,(-0.25,-0.75,0),(-0.75,-0.75,0),(-0.75,-0.75,0),(-0.5,-0.5,0)]
--myPoints = [(-0.2*k,0.2*k,-0.0)|k<-[-5..5]]




despliega2 = do windowSize $= Size 500 500
                currentColor $= Color4 1 0 0 0
                renderPrimitive Polygon $
                                mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z) myPoints
                flush



desplegar = do 
      windowSize $= Size 500 500
      clear [ColorBuffer]
      translate $ Vector3 (0.25) (0.25) (0.0 ::GLfloat)
      rotate 45 $ Vector3 0 0 (1::GLfloat)
      translate $ Vector3 (-0.25) (-0.25) (0.0 ::GLfloat)
      --triangulo (-0.5, -0.5, 0) (0.5,-0.5,0) (0,0.4,0)
      --loadIdentity
      cuadrado 0.5
      --triangulo (-0.5, -0.5, 0) (0.5,-0.5,0) (0,0.4,0)
      flush
puntosCuadrado:: GLfloat -> [(GLfloat,GLfloat,GLfloat)]
puntosCuadrado t = [(0,0,0), (0, t,0), (t, t, 0), (t,0,0)]
cuadro t =  renderPrimitive Polygon $ do
            currentColor $= Color4 1 0 0 1
            vertex$Vertex3 0 0 (0::GLfloat)
            vertex$Vertex3 0 t (0::GLfloat)
            currentColor $= Color4 0.2 0 0 1
            vertex$Vertex3 t t (0::GLfloat)
            vertex$Vertex3 t 0 (0::GLfloat)

cuadrado t = renderPrimitive Polygon $ do
    mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z) (puntosCuadrado t)

triangulo  (x1,y1,z1) (x2,y2,z2) (x3,y3,z3)=  renderPrimitive Polygon $ do
            currentColor $= Color4 1 0 0 1
            vertex$Vertex3 x1 y1  (0::GLfloat)
            currentColor $= Color4 0 1 0 1
            vertex$Vertex3 x2 y2 (0::GLfloat)
            currentColor $= Color4 0 0 1 1
            vertex$Vertex3 x3 y3 (0::GLfloat)
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



