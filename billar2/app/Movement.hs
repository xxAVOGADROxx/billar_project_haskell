module Movement where 
import Vec

gravity :: Float
gravity = 9.8

data Ball = Ball {identifier  :: Int,
        radio       :: Float,
        mass        :: Int,
        miu         :: Float,
        color       ::[Float],
        acceleration:: Vec,
        velocity    :: Vec,
        position    :: Vec} deriving (Show, Eq)


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

moveBall :: Float -> Ball -> Ball
moveBall t ball@(Ball _ _ _ _ _ (Vec acX acY) (Vec velX velY) (Vec posX posY))
    | acX/=0 || acY/=0 = ball{ acceleration=newAcc , velocity=newVel , position=newPos}
    |otherwise = ball{acceleration=(Vec 0 0) , velocity=(Vec 0 0)}
    where   newAcc = (frenarBall t ball)
            newVel = (Vec (velX + acX*t) (velY + acY*t))
            newPos = (Vec (posX + velX*t) (posY + velY*t))

acelBall :: Ball -> Ball
acelBall ball = ball{acceleration = scalarMult ((-1)*mi*gravity) (normalize vel)}
    where   mi = miu ball
            vel = velocity ball


frenarBall :: Float -> Ball -> Vec
frenarBall t (Ball _ _ _ _ _ (Vec accX accY) (Vec x y) _)
    | (abs x)< limX && (abs y) < limY = (Vec 0 0)
    | otherwise = (Vec accX accY)
    where   limX = 2* (abs $ accX * t)
            limY = 2* (abs $ accY * t)


choque :: Float -> Ball -> Ball -> Bool
choque t ball1@(Ball id1 rad1 _ _ _ _ _ pos1) ball2@(Ball id2 rad2 _ _ _ _ _ pos2)
    |(id1/=id2) && ((distance pos1 pos2) < (rad1+rad2)) && ((distance nPos1 nPos2) < (distance pos1 pos2))= True
    | otherwise = False
    where   (Ball _ _ _ _ _ _ _ nPos1) = moveBall t ball1
            (Ball _ _ _ _ _ _ _ nPos2) = moveBall t ball2


comprobarChoque :: Float -> Ball -> [Ball] -> [Ball]
comprobarChoque t ball1 balls = take 1 $ [ball | ball<-balls, choque t ball1 ball]


manageChoque :: Float -> Ball -> Ball -> [Ball] -> [Ball]
manageChoque t ball1 ball2 balls = reemplazarPelota newball2 $ reemplazarPelota newball1 balls
    where   [vel1,vel2] = cambiarVelocidadesPelotas ball1 ball2 
            newball1 = acelBall ball1{velocity=vel1}
            newball2 = acelBall ball2{velocity=vel2}

cambiarVelocidadesPelotas :: Ball -> Ball -> [Vec]
cambiarVelocidadesPelotas ball1@(Ball _ _ _ _ _ _ x pos1) ball2@(Ball _ _ _ _ _ _ _ pos2) = [newVelBall1,newVelBall2]
    where   y = distanceVector pos1 pos2
            alpha = (productoPunto x y)/(productoPunto y y)
            newVelBall2 = scalarMult alpha y 
            newVelBall1 = sumVec x (scalarMult (-1) newVelBall2)


iterationBall :: Float -> Ball -> [Ball] -> [Ball]
iterationBall t ball balls
    |choqueBalls == [] = reemplazarPelota (moveBall t ball) balls
    |otherwise = manageChoque t ball (head choqueBalls) balls
    where choqueBalls = comprobarChoque t ball balls


iteration :: Float -> [Ball] -> [Ball] -> [Ball]
iteration t [] balls = balls
iteration t pelotas balls
    |acX/=0 || acY/=0 = iterationBall t x (iteration t xs balls)
    |otherwise = (iteration t xs balls)
    where   (x:xs) = pelotas
            (Vec acX acY)= acceleration x

