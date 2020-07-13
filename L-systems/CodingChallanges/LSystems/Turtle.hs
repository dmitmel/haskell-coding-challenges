module CodingChallanges.LSystems.Turtle where

import Math.LinearAlgebra.Vector
import Math.LinearAlgebra.Vector.Vector2

data Turtle = Turtle
    { turtlePos   :: Vector2 Float
    , turtleDir   :: Vector2 Float
    , turtlePaths :: [TurtlePath]
    , turtleStack :: [(Vector2 Float, Vector2 Float)]
    } deriving (Read, Eq, Ord, Show)

type TurtlePath = [Vector2 Float]

type TurtleCommand = Turtle -> Turtle

createTurtle :: Turtle
createTurtle = Turtle vZero vRight [[vZero]] []

executeTurtleCmds :: [TurtleCommand] -> Turtle -> Turtle
executeTurtleCmds cmds turtle = foldl (\turtle' cmd -> cmd turtle') turtle cmds

turnLeft, turnRight :: Float -> TurtleCommand
turnLeft  angle turtle@Turtle {turtleDir = dir} = turtle{turtleDir = rotateDeg   angle  dir}
turnRight angle turtle@Turtle {turtleDir = dir} = turtle{turtleDir = rotateDeg (-angle) dir}

reverseDirection :: TurtleCommand
reverseDirection turtle@Turtle {turtleDir = dir} = turtle{turtleDir = -dir}

setDirection :: Float -> TurtleCommand
setDirection angle turtle = turtle{turtleDir = rotateDeg angle vRight}

move, jump :: Float -> TurtleCommand
move units turtle@(Turtle pos dir _ _) = pushPosition $
    turtle {turtlePos = pos + dir *^ units}
jump units turtle@(Turtle pos dir _ _) =
    turtle {turtlePos = pos + dir *^ units}

pushPosition :: TurtleCommand
pushPosition turtle@(Turtle pos _ []                 _) = turtle {turtlePaths = [[pos]]}
pushPosition turtle@(Turtle pos _ (path:closedPaths) _) = turtle {turtlePaths = (pos:path):closedPaths}

pushTransform, popTransform :: TurtleCommand
pushTransform (Turtle pos dir paths stack) = Turtle pos dir paths ((pos, dir):stack)
popTransform (Turtle _ _ _     []                ) = error "CodingChallanges.LSystems.Turtle.popTransform: stack is empty"
popTransform (Turtle _ _ paths ((pos, dir):stack)) = Turtle pos dir ([pos]:paths) stack
