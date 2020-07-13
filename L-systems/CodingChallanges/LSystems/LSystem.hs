{-# LANGUAGE OverloadedStrings #-}

module CodingChallanges.LSystems.LSystem where

import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as M
import Data.Aeson
import CodingChallanges.LSystems.Turtle

data LSystem = LSystem
    { systemName  :: String
    , systemAngle :: Float
    , systemAxiom :: String
    , systemRules :: M.Map Char String
    } deriving (Read, Eq, Ord, Show)

instance FromJSON LSystem where
    parseJSON = withObject "LSystem" $ \obj -> LSystem
        <$> obj .: "name"
        <*> obj .: "angle"
        <*> obj .: "axiom"
        <*> obj .: "rules"

generate :: LSystem -> [LSystem]
generate = iterate step

generateN :: Int -> LSystem -> LSystem
generateN n lsys
    | n > 0     = generateN (n - 1) $ step lsys
    | otherwise = lsys

step :: LSystem -> LSystem
step system@LSystem {systemAxiom = axiom, systemRules = rules} =
    system {systemAxiom = concatMap replaceChar axiom}
    where replaceChar c = fromMaybe [c] $ M.lookup c rules

systemTurtleCmds :: LSystem -> [TurtleCommand]
systemTurtleCmds (LSystem _ angle axiom _) = map (\c -> case c of
    '-' -> turnLeft angle
    '+' -> turnRight angle
    '|' -> reverseDirection
    '^' -> setDirection 90
    '>' -> setDirection 0
    '<' -> setDirection 180
    'v' -> setDirection 270
    'F' -> move 1
    'f' -> jump 1
    'G' -> move 1
    'g' -> jump 1
    '[' -> pushTransform
    ']' -> popTransform
    _   -> id) axiom
