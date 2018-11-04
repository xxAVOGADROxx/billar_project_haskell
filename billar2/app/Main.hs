-- | Billar Project
-- | Export libraries
import Graphics.Gloss as G
import Graphics.Gloss.Interface.Pure.Game
-- |Parameters

width, height, offset :: Int
gravity :: Float
width = 300
height = 300
offset = 100
gravity = 9.8


-- |Functions ----------------------------------------------------------------------------------------------------------
normalize :: Vec -> Vec
normalize (Vec x y) = Vec (x/r) (y/r)
            where r = sqrt (x^2 + y^2)

distance :: Ball -> Ball -> Float
distance ball1 ball2 = sqrt ((pos2X-pos1X)^2 + (pos2Y-pos1Y)^2)
    where   (Vec pos1X pos1Y) = position ball1
            (Vec pos2X pos2Y) = position ball2

distanceVector :: Ball -> Ball -> Vec
distanceVector ball1 ball2 = Vec (pos2X-pos1X) (pos2Y-pos1Y)
    where   (Vec pos1X pos1Y) = position ball1
            (Vec pos2X pos2Y) = position ball2

productoPunto :: Vec -> Vec -> Float
productoPunto (Vec x1 y1) (Vec x2 y2) = (x1*x2 + y1*y2)

choque :: Ball -> Ball -> Bool 
choque ball1 ball2 
    |(id1==id2) || ((distance ball1 ball2) > (rad1+rad2)) = False
    | otherwise = True
    where   rad1 = radio ball1
            rad2 = radio ball2
            id1 = identifier ball1
            id2 = identifier ball2

buscarPelota :: Int -> [Ball] -> Ball
buscarPelota id (x:xs)
    | id /= id2 = buscarPelota id xs
    | otherwise = x 
    where id2= identifier x

reemplazarPelota :: Ball -> [Ball] -> [Ball]
reemplazarPelota ball (x:xs)
    | id1==id2 = ball : xs
    | otherwise = x : (reemplazarPelota ball xs)
    where   id1 = identifier ball
            id2 = identifier x

comprobarChoque :: Ball -> [Ball] -> [Ball]
comprobarChoque ball1 balls = take 1 $ [ball | ball<-balls, choque ball1 ball]
    
scalarMult :: Float -> Vec -> Vec
scalarMult a (Vec x y) = Vec (a*x) (a*y)

sumVec :: Vec -> Vec -> Vec
sumVec (Vec x1 y1) (Vec x2 y2) = (Vec (x1+x2) (y1+y2))

acelBall :: Ball -> Ball
acelBall (Ball id rad mass miu ac velocity pos) = Ball id
                                                    rad
                                                    mass
                                                    miu
                                                    (scalarMult ((-1)*miu*gravity) (normalize velocity))
                                                    velocity
                                                    pos 


frenarAc :: Vec -> Vec -> Vec
frenarAc (Vec x y) (Vec acX acY)
    | (abs x)< 0.2 && (abs y) < 0.2 = (Vec 0 0)
    | otherwise = (Vec acX acY)
                                                    
-- | Initial window
window :: Display
window =  InWindow "Billiard Game" (height, width)(offset,offset)

background :: Color
background = white


initialWorld :: [Ball]
initialWorld = [acelBall (Ball 1 10 20 0.5 (Vec 0 0) (Vec (60) (24)) (Vec (-50) (-10))),
                (Ball 2 10 20 0.5 (Vec 0 0) (Vec 0 0) (Vec 10 20)),
                (Ball 3 10 20 0.5 (Vec 0 0) (Vec 0 0) (Vec 60 90)),
                (Ball 4 10 20 0.5 (Vec 0 0) (Vec 0 0) (Vec 50 100))] 


-- | Movement of the balls
drawOneBall :: Ball -> Picture
drawOneBall (Ball _ r _ _ _ _ (Vec x y) ) = translate x y $  color ballColor $ G.circleSolid r
                               where ballColor = red

toPicture :: [Ball] -> Picture
toPicture pelotas = pictures $ map drawOneBall pelotas
                        

moveBall :: Float -> Ball -> Ball
moveBall t (Ball id rad mass miu (Vec acX acY) (Vec velX velY) (Vec posX posY)) 
    | acX/=0 || acY/=0 = Ball id
                        rad
                        mass
                        miu
                        (frenarAc (Vec velX velY) (Vec acX acY))
                        (Vec (velX + acX*t) (velY + acY*t))
                        (Vec (posX + velX*t) (posY + velY*t))
    |otherwise = Ball id
                    rad
                    mass
                    miu
                    (Vec 0 0)
                    (Vec 0 0)
                    (Vec posX posY)

cambiarDirPelota :: Vec -> Ball -> Ball
cambiarDirPelota (Vec x y) (Ball id rad mass miu ac _ pos) = acelBall (Ball id rad mass miu ac (Vec x y) pos)

manageChoque :: Float -> Ball -> Ball -> [Ball] -> [Ball] 
manageChoque t ball1 ball2 balls = moverPelotasChoque t pelota1 pelota2 newpelotas
    where   newpelotas = cambiarVelocidadesPelotas ball1 ball2 balls
            id1 = identifier ball1
            id2 = identifier ball2
            pelota1 = buscarPelota id1 newpelotas
            pelota2 = buscarPelota id2 newpelotas

moverPelotasChoque :: Float -> Ball -> Ball -> [Ball] -> [Ball] 
moverPelotasChoque t ball1 ball2 balls 
    |choque ball1 ball2 = moverPelotasChoque t newpelota1 newpelota2 (reemplazarPelota newpelota1 (reemplazarPelota newpelota2 balls))
    |otherwise = balls
    where   newpelota1 = moveBall t ball1
            newpelota2 = moveBall t ball2



cambiarVelocidadesPelotas :: Ball -> Ball -> [Ball] -> [Ball]
cambiarVelocidadesPelotas ball1 ball2 balls = reemplazarPelota 
                                            (cambiarDirPelota newVelBall2 ball2) 
                                            (reemplazarPelota (cambiarDirPelota newVelBall1 ball1) balls)
    where   y = distanceVector ball1 ball2
            x = velocity ball1
            alpha = (productoPunto x y)/(productoPunto y y)
            newVelBall2 = scalarMult alpha y 
            newVelBall1 = sumVec x (scalarMult (-1) newVelBall2)



iterationBall :: Float -> Ball -> [Ball] -> [Ball]
iterationBall t ball balls
    |choqueBalls == [] = reemplazarPelota (moveBall t ball) balls
    |otherwise = manageChoque t ball (head choqueBalls) balls 
    where choqueBalls = comprobarChoque ball balls


iteration :: Float -> [Ball] -> [Ball] -> [Ball]
iteration t [] balls = balls
iteration t pelotas balls
    |acX/=0 || acY/=0 = iterationBall t x (iteration t xs balls)
    |otherwise = (iteration t xs balls)
    where   (x:xs) = pelotas 
            (Vec acX acY)= acceleration x
            

iterationWorld :: Float -> [Ball] -> [Ball]
iterationWorld t pelotas = iteration t pelotas pelotas



evento :: Event -> [Ball] -> [Ball]
evento _ x = x

-- | Main --------------------------------------------------------------------------------------------------------------
main = do play window
              background              --  Background color
              12                       -- Number of simulation steps to take for each second real time
              initialWorld            -- Initial world
              toPicture                 -- Convert the world a picture
              evento                  -- Function to handle input events
              iterationWorld              -- A function to step the world one iteration. It is passed the period of time (in seconds) needing to be advanced



-- | Types -------------------------------------------------------------------------------------------------------------

-- | The game world



-- | The billar's balls
data Ball = Ball {identifier  :: Int,
                  radio       :: Float,
                  mass        :: Int,
                  miu         :: Float,
                  acceleration:: Vec,
                  velocity    :: Vec,
                  position    :: Vec}
                  deriving (Show, Eq) 

data Table = Table {tableHeight :: Int,
                    tableWidth ::  Int }
                    deriving Show

-- | Vector for the billar's balls
data Vec = Vec {x :: Float,
                y :: Float}
                deriving (Show, Eq)
