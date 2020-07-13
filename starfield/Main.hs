{-
Starfield (https://youtu.be/17WoOqgXsRM?list=PLRqwX-V7Uu6ZiZxtDDRCi6uhfTH4FilpH)
-}
module Main where

import           Graphics.Gloss
import           Graphics.Gloss.Data.ViewPort
import           System.Random

-- Star is a 3D vector. Value of the Z-component is how far is a star from the camera
data Star = Star
  { starX :: Float
  , starY :: Float
  , starZ :: Float
  , starPrevZ :: Float
  } deriving (Read, Eq, Ord, Show)

data Starfield = Starfield
  { starfieldStars :: [Star]
    -- Random generator is for creating new stars on random positions
  , starfieldRandomGen :: StdGen
  } deriving (Read, Show)

-- This is how fast stars move around the camera
spaceshipSpeed :: Float
spaceshipSpeed = 500

starsCount :: Int
starsCount = 500

windowSize :: (Int, Int)
windowWidth, windowHeight :: Int
windowSize@(windowWidth, windowHeight) = (640, 640)

windowFloatWidth, windowFloatHeight :: Float
windowFloatWidth = fromIntegral windowWidth

windowFloatHeight = fromIntegral windowHeight

window :: Display
window = InWindow "Starfield" windowSize (50, 50)

background :: Color
background = black

fps :: Int
fps = 30

main :: IO ()
main = simulate window background fps createStarfield render update

createStarfield :: Starfield
createStarfield =
  let (stars, rndGen) = createStars starsCount $ mkStdGen 0
  in  Starfield stars rndGen
 where
  createStars :: Int -> StdGen -> ([Star], StdGen)
  createStars count rndGen
    | count > 0
    = let (star     , rndGen2) = createStar rndGen
          (nextStars, lastGen) = createStars (count - 1) rndGen2
      in  (star : nextStars, lastGen)
    | otherwise
    = ([], rndGen)

createStar :: StdGen -> (Star, StdGen)
createStar rndGen =
  let (x, rndGen2) =
        randomR (-windowFloatWidth / 2, windowFloatWidth / 2) rndGen
      (y, rndGen3) =
        randomR (-windowFloatHeight / 2, windowFloatHeight / 2) rndGen2
      (z, rndGen4) = randomR (0, windowFloatWidth / 2) rndGen3
  in  (Star x y z z, rndGen4)

render :: Starfield -> Picture
render (Starfield stars _) = pictures $ map renderStar stars
 where
  renderStar :: Star -> Picture
  renderStar (Star x y z prevZ) = color white $ line
    [ ( mapToRange (x / prevZ)
                   (-0.5                 , 0.5)
                   (-windowFloatWidth / 2, windowFloatWidth / 2)
      , mapToRange (y / prevZ)
                   (-0.5                  , 0.5)
                   (-windowFloatHeight / 2, windowFloatHeight / 2)
      )
    , ( mapToRange (x / z)
                   (-0.5                 , 0.5)
                   (-windowFloatWidth / 2, windowFloatWidth / 2)
      , mapToRange (y / z)
                   (-0.5                  , 0.5)
                   (-windowFloatHeight / 2, windowFloatHeight / 2)
      )
    ]
  mapToRange :: Float -> (Float, Float) -> (Float, Float) -> Float
  mapToRange n (min1, max1) (min2, max2) =
    min2 + (max2 - min2) * ((n - min1) / (max1 - min1))

update :: ViewPort -> Float -> Starfield -> Starfield
-- Delta time is the time in seconds between previous and current frames
update _ deltaTime (Starfield stars rndGen) = foldl updateStar
                                                    (Starfield [] rndGen)
                                                    stars
 where
  updateStar :: Starfield -> Star -> Starfield
  updateStar (Starfield stars rndGen) (Star x y z _)
    | newZ < 1
    = let (star, rndGen2) = createStar rndGen
      in  Starfield (star : stars) rndGen2
    | otherwise
    = Starfield (Star x y newZ z : stars) rndGen
          {-
            Multiplying value by delta time gives speed in value per second.
            For example, following code will move pseudo-object by 10 units per second:

                object.position = object.position + 10 * deltaTime

            And this speed is same for any FPS (probably)
            -}
    where newZ = z - spaceshipSpeed * deltaTime
