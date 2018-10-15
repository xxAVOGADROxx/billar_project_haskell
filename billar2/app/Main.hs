-- | Billar Project
-- | Export libraries
import Graphics.Gloss as G
import Graphics.Gloss.Interface.Pure.Game
-- |Parameters

width, height, offset :: Int
width = 300
height = 300
offset = 100

-- |Functions ----------------------------------------------------------------------------------------------------------

-- | Initial window
window :: Display
window =  InWindow "Billiard Game" (height, width)(offset,offset)

background :: Color
background = white



initialWorld :: Ball
initialWorld = Ball 1 10 20 0.1 2


-- | Movement of the balls
toPicture :: Ball -> Picture
toPicture (Ball _ r _ _ _ ) = translate (-20)(100) $  color ballColor $ G.circleSolid r
                               where ballColor = red




iteration :: Float -> Ball -> Ball
iteration _ b = b



evento :: Event -> Ball -> Ball
evento _ x = x

-- | Main --------------------------------------------------------------------------------------------------------------
main = do play window
              background              --  Background color
              2                       -- Number of simulation steps to take for each second real time
              initialWorld            -- Initial world
              toPicture                 -- Convert the world a picture
              evento                  -- Function to handle input events
              iteration              -- A function to step the world one iteration. It is passed the period of time (in seconds) needing to be advanced



-- | Types -------------------------------------------------------------------------------------------------------------

-- | The game world



-- | The billar's balls
data Ball = Ball {identifier  :: Int,
                  radio       :: Float,
                  mass        :: Int,
                  miu         :: Float,
                  acceleration:: Float}
                  --position    :: Vec2}
                  --velocity    :: Vec2}
                  deriving Show

data Table = Table {tableHeight :: Int,
                    tableWidth ::  Int }
                    deriving Show

-- | Vector for the billar's balls
data Vec2 = Vec2 {x :: Double,
                    y :: Double}
                    deriving Show
