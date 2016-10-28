module Circle where                  
import PointsForRendering      
import Graphics.Rendering.OpenGL 


circlePoints radius number = [let alpha = twoPi * i /number 
    in  (radius*(sin (alpha)) ,radius * (cos (alpha)),0)  
   |i <- [1,2..number]]
  where
    twoPi = 2*pi

