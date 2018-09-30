
-- | Display "Hello World" in a window.
import Graphics.Gloss



-- | Main --------------------------------------------------------------------------------------------------------------
main
 = do let w = World {width      = maxx - minx
                    , height    = maxy - miny
                    , pixWidth  = 700
                    , pixHeight = 700 }
   simulate (IntWindow "Billar Project" ())


 display
        (InWindow "Hello World" (400, 150) (0, 0))-- windows size, window position
	       white			 -- background color
	      picture			 -- picture to display

picture
	= Translate (-170) (-20) -- shift the text to the middle of the window
	$ Scale 0.5 0.5		 -- display it half the original size
	$ Text "Hello World"	 -- text to display

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

data Vec2 = Vect2 {x :: Double,
                    y :: Double}
                    deriving Show