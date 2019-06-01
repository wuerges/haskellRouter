{-# LANGUAGE DuplicateRecordFields, OverloadedStrings #-}

module Parser where

import Data.Attoparsec.Text
import Data.Word

data Point = Point { x :: Int, y :: Int } deriving Show
data Rect = Rect { left :: Point, right :: Point } deriving Show

data Shape = Shape { layer :: Int, rect :: Rect } deriving Show
data Via = Via { layer :: Int, point :: Point } deriving Show

data Settings = Settings {
  spacing :: Int,
  boundary :: Rect,
  routedVias :: Int,
  obstacles :: Int,
  metalLayers :: Int,
  routedShapes :: Int
} deriving Show

data Input = Input {
  settings :: Settings ,
  routedShapes :: [Shape],
  routedVias :: [Via],
  obstacles :: [Shape]
}

parseInput :: Parser Input
parseInput = do
  vc <- parseViaCost
  sp <- parseSpacing
  ml <- parseMetalLayers
  rs <- parseRoutedShapes
  rv <- parseRoutedVias
  os <- parseObstacles
  let x = Settings vc sp ml rs rv os
  rShapes <- many' parseRoutedShape
  rVias <- many' parseRoutedVia
  obst <- many' parseObstacle
  return $ Input x rShapes rVias obst

parseBoundary :: Parser Rect
parseBoundary = do
  string "Boundary"
  skipSpace
  parseRect

-- parseParam :: a -> Parser Int
parseParam txt = do
  string txt
  skipSpace
  decimal

parseSpacing = do
  string "Spacing"
  skipSpace
  parseRect


parseViaCost = parseParam "ViaCost"
parseMetalLayers = parseParam "#MetalLayers"
parseRoutedShapes = parseParam "#RoutedShapes"
parseRoutedVias = parseParam "#RoutedVias"
parseObstacles = parseParam "#Obstacles"

parseRect :: Parser Rect
parseRect = do
  p1 <- parsePoint
  skipSpace
  p2 <- parsePoint
  return $ Rect p1 p2

parseShape :: Parser Shape
parseShape = do
  l <- parseLayer
  skipSpace
  r <- parseRect
  return $ Shape l r

parseRoutedShape :: Parser Shape
parseRoutedShape = do
  string "RoutedShape"
  skipSpace
  parseShape

parseRoutedVia :: Parser Via
parseRoutedVia = do
  string "RoutedVia"
  skipSpace
  char 'V'
  l <- decimal
  skipSpace
  p <- parsePoint
  return $ Via l p

parseObstacle :: Parser Shape
parseObstacle = do
  string "Obstacle"
  skipSpace
  parseShape

parseLayer :: Parser Int
parseLayer = char 'M' >> decimal

parsePoint = do
  char '('
  skipSpace
  n1 <- decimal
  skipSpace
  char ','
  skipSpace
  n2 <- decimal
  skipSpace
  char ')'
  return $ Point n1 n2

testFunc :: IO ()
testFunc = do
  print $ parseOnly parseRoutedShape "RoutedShape M2 (694,482) (700,517)"
  print $ parseOnly parseObstacle "Obstacle M3 (6985,0) (7000,168)"
  print $ parseOnly parsePoint "(694,482)"
  print $ parseOnly parseLayer "M1"
  print $ parseOnly parseRoutedVia "RoutedVia V1 (648,1955)"
