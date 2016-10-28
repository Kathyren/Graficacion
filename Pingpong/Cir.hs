import PointsForRendering
import Circle
import Graphics.Rendering.OpenGL

main = renderInWindow $ do
            clear [ColorBuffer]
            renderCircleApprox 0.8 10

            --fillCircle 0.8 


--renderCircle r = displayPoints (circle r) LineLoop
--fillCircle r = displayPoints (circle r) Polygon
