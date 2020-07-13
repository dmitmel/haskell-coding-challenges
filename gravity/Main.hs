{-
Gravity
-}
module Main where

import           Graphics.Gloss
import           Graphics.Gloss.Interface.Pure.Game
import           System.Random
import           Data.Maybe
import           Math
import           Math.LinearAlgebra.Vector
import           Math.LinearAlgebra.Vector.Vector2

type FVector2 = Vector2 Float

data Space = Space
    { spacePlanets         :: [Planet]
    , spaceSize            :: (FVector2, FVector2)
    , spaceTimeScale       :: Float
    , spaceTimeScaleChange :: TimeScaleChange
    } deriving (Read, Eq, Ord, Show)

data TimeScaleChange = Slower | NoChange | Faster deriving (Read, Eq, Ord, Show)

data Planet = Planet
    { planetPosition :: FVector2
    , planetRadius   :: Float
    , planetVelocity :: FVector2
    , planetMass     :: Float
    } deriving (Read, Eq, Ord, Show)

gravity, drag, bounciness :: Float
-- gravity = 1e4
gravity = 1e3
drag = 1
bounciness = 1

planetsCount :: Int
planetsCount = 50
-- planetsCount = 2

planetMaxInitialSpeed :: Float
planetMaxInitialSpeed = 0
-- planetMaxInitialSpeed = 1000

planetRadiusRange :: (Float, Float)
planetRadiusRange = (10, 20)

antimaterProbability :: Float
-- antimaterProbability = 0.5
antimaterProbability = 0

planetDensity :: Float
planetDensity = 0.001
-- planetDensity = 0.002

windowSize :: (Int, Int)
windowSize = (900, 600)

window :: Display
window = InWindow "Gravity" windowSize (50, 50)

background :: Color
background = greyN 0.5

planetColor :: Color
planetColor = white

fps :: Int
fps = 100

main :: IO ()
main = do
  let _spaceSize = getSpaceSize windowSize
  planets <- getStdRandom $ createPlanets planetsCount _spaceSize
  let space = Space planets _spaceSize 1 NoChange
  play window background fps space render handleEvent update

getSpaceSize :: (Int, Int) -> (FVector2, FVector2)
getSpaceSize (windowWidth, windowHeight) =
  ( V2 (-windowFloatWidth / 2) (-windowFloatHeight / 2)
  , V2 (windowFloatWidth / 2)  (windowFloatHeight / 2)
  )
 where
  windowFloatWidth  = fromIntegral windowWidth
  windowFloatHeight = fromIntegral windowHeight

createPlanets :: Int -> (FVector2, FVector2) -> StdGen -> ([Planet], StdGen)
createPlanets count _spaceSize rndGen
  | count > 0
  = let (position , rndGen2)   = randomR _spaceSize rndGen
        (radius   , rndGen3)   = randomR planetRadiusRange rndGen2
        (velocity , rndGen4)   = randomInUnitCircle rndGen3
        (antimater, rndGen5)   = random rndGen4 :: (Float, StdGen)
        mass = planetDensity * 4 * pi * (radius * radius * radius) / 3
        (nextPlanets, lastGen) = createPlanets (count - 1) _spaceSize rndGen5
    in  ( Planet position
                 radius
                 (velocity *^ planetMaxInitialSpeed)
                 (if antimater <= antimaterProbability then -mass else mass)
          : nextPlanets
        , lastGen
        )
  | otherwise
  = ([], rndGen)

render :: Space -> Picture
render (Space planets _ timeScale _) =
  pictures
    $  concatMap (\planet -> [renderPlanet planet
           -- , renderUnscaledPlanetSpeed planet
           -- , renderPlanetSpeed planet
                                                 ]) planets
    ++ [scale 0.1 0.1 $ text $ show timeScale]
 where
  renderPlanet (Planet (V2 x y) radius _ mass) =
    translate x y
      $ color (if mass > 0 then white else black)
      $ circleSolid
      $ abs radius

  renderUnscaledPlanetSpeed (Planet (V2 x y) _ (V2 vx vy) _) =
    color red $ line [(x, y), (x + vx, y + vy)]

  renderPlanetSpeed (Planet (V2 x y) _ (V2 vx vy) _) =
    color blue $ line [(x, y), (x + vx * timeScale, y + vy * timeScale)]

handleEvent :: Event -> Space -> Space
handleEvent (EventKey (Char '=') Down _ _) space =
  space { spaceTimeScaleChange = Faster }
handleEvent (EventKey (Char '-') Down _ _) space =
  space { spaceTimeScaleChange = Slower }
handleEvent (EventKey _ Up _ _) space =
  space { spaceTimeScaleChange = NoChange }
handleEvent (EventResize newWindowSize) state =
  state { spaceSize = getSpaceSize newWindowSize }
handleEvent _ state = state

update :: Float -> Space -> Space
-- Delta time is the time in seconds between previous and current frames
update unscaledDeltaTime space@(Space planets _spaceSize timeScale timeScaleChange)
  = space
    { spacePlanets   = map updatePlanet planets
    , spaceTimeScale = case timeScaleChange of
                         NoChange -> timeScale
                         Slower   -> timeScale - unscaledDeltaTime
                         Faster   -> timeScale + unscaledDeltaTime
    }
 where
  deltaTime = unscaledDeltaTime * timeScale
  updatePlanet :: Planet -> Planet
  updatePlanet planet@(Planet position radius velocity mass) =
    let
      otherPlanets = filter (/= planet) planets
      acceleration = sum $ map (planetAttractionVector planet) otherPlanets
      velocityWithAcceleration = velocity + acceleration *^ deltaTime
      velocityWithDrag =
        velocityWithAcceleration *^ clamp01 (1 - drag * deltaTime)
      collisions             = mapMaybe (checkCollision planet) otherPlanets
      velocityWithCollisions = foldl
        (\v collision -> reflect collision v *^ bounciness)
        velocityWithDrag
        collisions
      newPosition = position + velocityWithCollisions *^ deltaTime
    in
      Planet (repeatV _spaceSize newPosition) radius velocityWithCollisions mass

  planetAttractionVector :: Planet -> Planet -> FVector2
  planetAttractionVector planet1@(Planet position1 _ _ _) planet2@(Planet position2 _ _ _)
    = (position1 `direction` position2) *^ planetAttractionForce planet1 planet2

  checkCollision :: Planet -> Planet -> Maybe FVector2
  checkCollision (Planet position1 radius1 _ _) (Planet position2 radius2 _ _)
    | currentDistance <= minDistance = Just (position1 `direction` position2)
    | otherwise                      = Nothing
   where
    currentDistance = position1 `sqrDistance` position2
    minDistance     = (radius1 + radius2) * (radius1 + radius2)

planetAttractionForce :: Planet -> Planet -> Float
planetAttractionForce (Planet position1 _ _ mass1) (Planet position2 _ _ mass2)
  = (mass1 * mass2) / (position1 `sqrDistance` position2) * (-gravity)
