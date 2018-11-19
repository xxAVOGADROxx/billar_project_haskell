module Main where
-- | Billar Project

-- | Export libraries
import Graphics.Gloss.Data.Color as C
import Graphics.Gloss.Data.Picture as G
import Graphics.Gloss.Interface.Pure.Game
import Movement
import Vec

-- |Parameters
width, height, offset :: Int

width = 300
height = 300
offset = 100


-- | Initial window
window :: Display
window =  InWindow "Billiard Game" (height, width)(offset,offset)

background :: Color
background = white



-- | data
-- initial state of the game:balls, taco, walls

initialGame :: State
initialGame = State initialBalls initialTaco

--initial items in the game
initialTaco :: Taco
initialTaco = Taco (Vec 0 0) 15 (Vec 10 50) (Vec 15 200) 10


initialBalls :: [Ball]
initialBalls = [acelBall (Ball 1 10 20 0.5 [1,0,0,1] (Vec 0 0) (Vec (60) (24)) (Vec (-50) (-10))),
                (Ball 2 10 20 0.5 [0,1,0,1] (Vec 0 0) (Vec 0 0) (Vec 10 20)),
                (Ball 3 10 20 0.5 [0,0,1,1] (Vec 0 0) (Vec 0 0) (Vec 60 90)),
                (Ball 4 10 20 0.5 [1,0,1,1] (Vec 0 0) (Vec 0 0) (Vec 50 100))]

initialTable :: Int
initialTable = undefined

initialWalls :: Int
initialWalls = undefined

-- | Converting the game state in a picture (Rendering process)
-- | Ball
drawOneBall :: Ball -> Picture
drawOneBall (Ball _ r _ _ col _ _ (Vec x y) ) = translate x y $  G.color ballColor $ G.circleSolid r
                               where ballColor = C.makeColor (col!!0) (col!!1) (col!!2) (col!!3)


toPicture :: State -> Picture
toPicture (State pelotas (Taco z p (Vec x y) (Vec a b) r )) = pictures $  map drawOneBall pelotas ++ toPictureTaco p x y a b r z


toPictureTaco :: Float -> Float -> Float -> Float -> Float -> Float -> Vec -> [Picture]
toPictureTaco p x y a b r z =  [ translate x y $ rotate r $ G.color( mixColors 40 60 green red) $ G.rectangleUpperSolid a b] ++ [ translate x y $ rotate (r+180) $ G.color black  $ G.rectangleUpperSolid a p]

-- | Movement of the balls


-- Event and Iteration functions 
iterationGame :: Float -> State -> State
iterationGame t (State pelotas taco) = State (iteration t pelotas pelotas) taco




handleEvent :: Event -> State -> State
handleEvent event state
  | EventMotion (x,y) <- event,
    State pelotas (Taco z p (Vec d e) (Vec a b) r ) <- state
  = State pelotas (Taco z  p (Vec x y) (Vec a b) r)
  | EventKey (MouseButton LeftButton) Down _ (x,y)  <- event,
    State pelotas (Taco z  p (Vec d e) (Vec a b) r ) <- state
  = State pelotas (Taco z  p (Vec x y) (Vec a b) (r+5))
  | EventKey (MouseButton RightButton) Down _ (x,y)  <- event,
    State pelotas (Taco z  p (Vec d e) (Vec a b) r ) <- state
  = State pelotas (Taco z  p (Vec x y) (Vec a b) (r-5))
  | EventKey (MouseButton WheelUp ) Down _ (x,y) <- event,
    State pelotas (Taco z  p (Vec d e) (Vec a b) r ) <- state
  = State pelotas (Taco z  p (Vec  d e)   (Vec a (controlSizeTacoUp b)) r )
  | EventKey (MouseButton WheelDown ) Down _ (x,y) <- event,
    State pelotas (Taco z  p (Vec d e) (Vec a b) r ) <- state
  = State pelotas (Taco z  p (Vec  d e)   (Vec a (controlSizeTacoDown b)) r )
  | EventKey (Char 'q') Down _ (x,y) <- event,
    State  pelotas (Taco z  p (Vec d e) (Vec a b) r ) <- state
  = State pelotas (Taco  z (p+10) (Vec x y) (Vec a 190) r )
  | EventKey (Char 'q') Up _ (x,y) <- event,
    State  pelotas (Taco  z p (Vec d e) (Vec a b) r ) <- state
  = State pelotas (Taco  z (p-10) (Vec x y) (Vec a 200) r )
  | otherwise = state

controlSizeTacoUp :: Float -> Float
controlSizeTacoUp x
  | x > 300 = 300
  | otherwise = x+5

controlSizeTacoDown :: Float -> Float
controlSizeTacoDown x
  | x < 200 = 200
  | otherwise = x-5

-- | Main --------------------------------------------------------------------------------------------------------------
main :: IO()
main = do play window
               background           --  Background color
               12                       -- Number of simulation steps to take for each second real time
               initialGame             -- Initial world
               toPicture                 -- Convert the world a picture
               handleEvent                 -- Function to handle input events
               iterationGame             -- A function to step the world one iteration. It is passed the period of time (in seconds) needing to be advanced



-- | Types -------------------------------------------------------------------------------------------------------------

-- | The game world
data State = State {pool ::  [Ball],
                    taco :: Taco
                   }deriving(Show, Eq)

-- | The taco data
data Taco = Taco {accel :: Vec,
                  referenceball :: Float,
                  positiontaco :: Vec,
                  size  :: Vec,
                  angle :: Float
                 }deriving (Show, Eq)




-- | The billar's balls


data Table = Table {tableHeight :: Int,
                    tableWidth ::  Int }
                    deriving Show

