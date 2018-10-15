-- | Billar Project
-- | Export libraries
import Graphics.Gloss as G
import Graphics.Gloss.Interface.Pure.Game
-- |Parameters

-- |Functions ----------------------------------------------------------------------------------------------------------
-- | Movement of the balls
movementBall :: Ball -> Picture
movementBall (Ball _ r _ _ _) = G.circleSolid r


iteracion :: Float -> Ball -> Ball
iteracion _ b = b


evento :: Event -> Ball -> Ball
evento _ x = x

-- | Main --------------------------------------------------------------------------------------------------------------
main = do play (InWindow "Billar Project" (700, 700)(20,20))
              white
              2
              (Ball 1 20.0 3 0.5 2.0)
              movementBall
              evento
              iteracion



-- | Types -------------------------------------------------------------------------------------------------------------

-- | The game world
data World
        = World
        { width         :: Double
        , height        :: Double
        , pixWidth      :: Int
        , pixHeight     :: Int }
         deriving Show


-- | The billar's balls
data Ball = Ball {identifier  :: Int,
                  radio       :: Float,
                  mass        :: Int,
                  miu         :: Float,
                  acceleration:: Float}
                --  position    :: Vec2,
                  --velocity    :: Vec2}
                   deriving Show

-- | Vector for the billar's balls
data Vec2 = Vec2 {x :: Double,
                    y :: Double}
                    deriving Show


