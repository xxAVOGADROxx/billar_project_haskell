-- | Billar Project
-- | Export libraries
import Graphics.Gloss

-- |Parameters



-- | Main --------------------------------------------------------------------------------------------------------------
main
 = do let w = World {width      = maxx - minx
                    , height    = maxy - miny
                    , pixWidth  = 700
                    , pixHeight = 700 }
   play (IntWindow "Billar Project" (700, 700)(20,20))
        (white)
        30
        0
        (circleSolid 3)




-- | Types -------------------------------------------------------------------------------------------------------------


data World
        = World
        { width         :: Double
        , height        :: Double
        , pixWidth      :: Int
        , pixHeight     :: Int }
         deriving Show

data Ball = Ball {identifier  :: Int,
                  radio       :: Float,
                  mass        :: Int,
                  miu         :: Float,
                  acceleration:: Float,
                  position    :: Vec2,
                  velocity    :: Vec2}
                   deriving Show

data Vec2 = Vec2 {x :: Double,
                    y :: Double}
                    deriving Show