import Graphics.UI.GLUT
import Data.IORef
_TIEMPO=0.00001
main = do
       getArgsAndInitialize
       createWindow "Graficos"
       tiempo <- newIORef 0.0
       tras <- newIORef 0.0
       dir <- newIORef 0.1
       alt <- newIORef 0.0
       reg <- newIORef 0.0
       displayCallback $= desplegar tras
       keyboardMouseCallback $= Just (trasladar dir)
       idleCallback $= Just (temporizador tras tiempo dir alt reg)
       mainLoop

temporizador tras tiempo dire altu reg= do
        t <- get tiempo
        dir <- get dire
        f<- get tras
       --dir <- dire
        if t>=1.0 then do
                       e<-get tras
                       r<-get reg
                       if e < -0.4 then reg $= 1
                       else if e > 0.3 then reg $= 0
                            else reg $= r
     
                       if r == 1 then tras $=f + dir
                       else tras $=f - dir
                       tiempo $= 0.0
                       postRedisplay Nothing
         else do
             tiempo $= t+_TIEMPO
             return ()
        

myPoints :: [(GLfloat,GLfloat,GLfloat)]
myPoints =  [(0.6,0.8,0),
             (0.6,0.2,0),(0.4,0.2,0),
             (0.4,0.6,0),(0.2,0.6,0),(0.2,0.8,0)]

eje::[GLfloat]->[(GLfloat,GLfloat,GLfloat)]
eje (g:gs) = [(x,g,0)|x<-[-1,1]]++[(g,y,0)|y<-[-1,1]]++eje gs
eje [] = [] 

ejes::[(GLfloat,GLfloat,GLfloat)]
ejes = eje (puntos (-0.001) 0.001 0.001)

reglas::[(GLfloat,GLfloat,GLfloat)]
reglas = eje (puntos (-1) 1 0.1)

despliegaEjes = do renderPrimitive Lines $ 
                       mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z) ejes
despliegaReglas = do renderPrimitive Lines $ 
                       mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z) reglas

puntos::GLfloat->GLfloat->GLfloat->[GLfloat]
puntos i f p | i < f = [i]++puntos (i+p) f p
             | otherwise = [f]

triangulo (x1,y1,z1) (x2,y2,z2) (x3,y3,z3)= 
            renderPrimitive Polygon $ do
            currentColor $= Color4 1 0 0 1
            vertex$Vertex3 x1 y1 (z1::GLfloat)
            currentColor $= Color4 0 1 0 1
            vertex$Vertex3 x2 y2 (z2::GLfloat)
            currentColor $= Color4 0 0 1 1
            vertex$Vertex3 x3 y3 (z3::GLfloat)

desplegar tras = do 
                        clear [ColorBuffer]
                        currentColor $= Color4 1 0 0 1
                        e<-get tras
                        loadIdentity
                        translate $ Vector3 e 0 (0.0 ::GLfloat)
                        triangulo (0,0.8,0) (-0.5,0,0) (0.5,0,0)
                        flush
trasladar  dir(SpecialKey KeyRight) Down _ _ = do
                      dir $= 0.1
trasladar  dir(SpecialKey KeyLeft) Down _ _ = do
                      dir $= -0.1
trasladar  alt(SpecialKey KeyUp) Down _ _ = do
                      alt $= 0.1
trasladar  alt(SpecialKey KeyDown) Down _ _ = do
                      alt $= -0.1
trasladar  alt(SpecialKey KeyDown) _ _ _ = do
                      return ()

