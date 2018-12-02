module Main where
-- | Billar Project

-- | Export libraries
import Graphics.Gloss.Data.Color as C
import Graphics.Gloss.Data.Picture as G
import Graphics.Gloss.Interface.Pure.Game
import Movement
import Vec

-- |Parameters

offset :: Int
offset = 200

-- | Initial window
window :: Display
window =  InWindow "Billiard Game" (round width, round height)(offset,offset)

background :: Color
background = white



-- | data
-- initial state of the game:balls, taco, walls

initialGame :: State
initialGame = State initialBalls initialTaco initialTable

--initial items in the game
initialTaco :: Taco
initialTaco = Taco (Vec 0 0) 15 (Vec 10 50) (Vec 15 200) 10


initialBalls :: [Ball]
initialBalls = [acelBall (Ball 1 10 20 0.5 [1,0,0,1] (Vec 0 0) (Vec (120) (48)) (Vec (-50) (-10))),
                (Ball 2 10 20 0.5 [0,1,0,1] (Vec 0 0) (Vec 0 0) (Vec 10 20)),
                (Ball 3 10 20 0.5 [0,0,1,1] (Vec 0 0) (Vec 0 0) (Vec 60 90)),
                (Ball 4 10 20 0.5 [1,0,1,1] (Vec 0 0) (Vec 0 0) (Vec 0 0))]


initialTable :: Table
initialTable = (Table width height)


-- | Converting the game state in a picture (Rendering process)
-- | Ball

newSize :: (Int,Int) -> State -> State
newSize (a,b) state
  | rx < ry = state {table = (Table na ((na*height)/width))}
  | otherwise = state {table = (Table ((nb*width)/height) nb ) }
  where na = fromIntegral a
        nb = fromIntegral b
        rx = na/width
        ry = nb/height


drawOneBall :: Ball -> Picture
drawOneBall (Ball _ r _ _ col _ _ (Vec x y) ) = translate x y $  G.color ballColor $ G.circleSolid r
                               where ballColor = C.makeColor (col!!0) (col!!1) (col!!2) (col!!3)

toPictureTaco :: Table -> Taco -> [Picture]
toPictureTaco (Table w h) (Taco z p (Vec x y) (Vec a b) r ) = [ translate x y $ Scale (w/width) (h/height) $ rotate r $ G.color( mixColors 40 60 green red) $ G.rectangleUpperSolid a b] ++ [ translate x y $ Scale (w/width) (h/height) $ rotate (r+180) $ G.color black  $ G.rectangleUpperSolid a p]
                               
adjuste :: Table -> Picture -> Picture
adjuste (Table w h) pic = Scale (w/width) (h/height) pic

toPicture :: State -> Picture
toPicture (State pelotas tac tab) = pictures $ [adjuste tab (pictures $ mesa ++ map drawOneBall pelotas)] ++ (toPictureTaco tab tac)
  where mesa = [G.color (C.makeColor 0 0.25 0 1)  $rectangleSolid width height]


-- | Movement of the balls


-- Event and Iteration functions 
iterationGame :: Float -> State -> State
iterationGame t (State pelotas taco table) = State (iteration t pelotas pelotas) taco table




handleEvent :: Event -> State -> State
handleEvent event state
  | EventMotion (x,y) <- event,
    State pelotas (Taco z p (Vec d e) (Vec a b) r ) table <- state
  = State pelotas (Taco z  p (Vec x y) (Vec a b) r) table
  | EventKey (MouseButton LeftButton) Down _ (x,y)  <- event,
    State pelotas (Taco z  p (Vec d e) (Vec a b) r ) table <- state
  = State pelotas (Taco z  p (Vec x y) (Vec a b) (r+5)) table 
  | EventKey (MouseButton RightButton) Down _ (x,y)  <- event,
    State pelotas (Taco z  p (Vec d e) (Vec a b) r ) table <- state
  = State pelotas (Taco z  p (Vec x y) (Vec a b) (r-5)) table
  | EventKey (MouseButton WheelUp ) Down _ (x,y) <- event,
    State pelotas (Taco z  p (Vec d e) (Vec a b) r ) table <- state
  = State pelotas (Taco z  p (Vec  d e)   (Vec a (controlSizeTacoUp b)) r ) table
  | EventKey (MouseButton WheelDown ) Down _ (x,y) <- event,
    State pelotas (Taco z  p (Vec d e) (Vec a b) r ) table <- state
  = State pelotas (Taco z  p (Vec  d e)   (Vec a (controlSizeTacoDown b)) r ) table
  | EventKey (Char 'q') Down _ (x,y) <- event,
    State  pelotas (Taco z  p (Vec d e) (Vec a b) r ) table <- state
  = State pelotas (Taco  z (p+10) (Vec x y) (Vec a 190) r ) table
  | EventKey (Char 'q') Up _ (x,y) <- event,
    State  pelotas (Taco  z p (Vec d e) (Vec a b) r ) table <- state
  = State pelotas (Taco  z (p-10) (Vec x y) (Vec a 200) r ) table
  | EventResize nSize <- event
  = newSize nSize state 
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
                    taco :: Taco,
                    table :: Table
                   }deriving(Show, Eq)

-- | The taco data
data Taco = Taco {accel :: Vec,
                  referenceball :: Float,
                  positiontaco :: Vec,
                  size  :: Vec,
                  angle :: Float
                 }deriving (Show, Eq)

-- | The billar's balls


data Table = Table {tableWidth ::  Float,
                    tableHeight :: Float
                  }deriving (Show, Eq)

