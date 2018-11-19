module Vec where 

data Vec = Vec {x :: Float,
                y :: Float}
            deriving (Show, Eq)

normalize :: Vec -> Vec
normalize (Vec x y) = Vec (x/r) (y/r)
        where r = sqrt (x^2 + y^2)
            
distance :: Vec -> Vec -> Float
distance (Vec pos1X pos1Y) (Vec pos2X pos2Y) = sqrt ((pos2X-pos1X)^2 + (pos2Y-pos1Y)^2)

distanceVector ::  Vec -> Vec -> Vec
distanceVector (Vec pos1X pos1Y) (Vec pos2X pos2Y) = Vec (pos2X-pos1X) (pos2Y-pos1Y)

scalarMult :: Float -> Vec -> Vec
scalarMult a (Vec x y) = Vec (a*x) (a*y)
            
sumVec :: Vec -> Vec -> Vec
sumVec (Vec x1 y1) (Vec x2 y2) = (Vec (x1+x2) (y1+y2))

productoPunto :: Vec -> Vec -> Float
productoPunto (Vec x1 y1) (Vec x2 y2) = (x1*x2 + y1*y2)
            