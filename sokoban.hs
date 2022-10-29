{-# LANGUAGE OverloadedStrings #-}
import CodeWorld

cloud :: Picture
cloud = pictures [translated x y (colored white (solidCircle 0.15))
  | x <- [-0.25, 0, 0.25], y <- [0]]
  & pictures [translated x y (colored white (solidCircle 0.15))
  | x <- [0], y <- [-0.15, 0.15]]

blankField :: Picture
blankField = cloud
  & colored (lighter 0.3 blue) (solidRectangle 1 1)

wall :: Picture
wall = colored (darker 0.2 brown) (solidRectangle 1 1)

ground :: Picture
ground = pictures [translated x y (colored orange (solidCircle 0.04))
  | x <- [-0.4, -0.3 .. 0.4], y <- [-0.4, -0.3 .. 0.4] ]
  & colored (lighter 0.2 brown) (solidRectangle 1 1)

storage :: Picture
storage = colored red (solidCircle 0.1) 
  & colored (lighter 0.1 red) (solidCircle 0.2) 
  & ground

box :: Bool -> Picture
box isReady
  | True = colorBox (mixed [orange, red])
  | False = colorBox yellow

colorBox :: Color -> Picture
colorBox c = rectangle 1 1 
  & polyline([(-0.5, 0.5), (0.5, -0.5)])
  & polyline([(0.5, 0.5), (-0.5, -0.5)])
  & colored c (solidRectangle 1 1)

data Tile = Wall | Ground | Storage | Box | Blank

drawTile :: Tile -> Picture
drawTile Wall = wall
drawTile Ground = ground
drawTile Storage = storage
drawTile Box = box False
drawTile Blank = blankField

data Coord = C {x, y :: Integer}
initialState :: (Direction, Coord)
initialState = (U, C 0 1)

maze :: Coord -> Tile
maze c
  | abs (x c) > 4  || abs (y c) > 4 = Blank
  | abs (x c) == 4 || abs (y c) == 4 = Wall
  | (x c) ==  2 && (y c) <= 0 = Wall
  | (x c) ==  3 && (y c) <= 0 = Storage
  | (x c) >= -2 && (y c) == 0 = Box
  | otherwise = Ground

pictureOfMaze :: Picture
pictureOfMaze = pictures [translated (fromIntegral x) (fromIntegral y) (drawTile (maze (C x y))) 
  | x <- [-10 .. 10], y <- [-10 .. 10]]
  
player1 :: Picture
player1 = polyline [(0, -0.4), (0, 0.4)] & polyline [(-0.2, 0.2), (0, 0.4), (0.2, 0.2)]

data Direction = R | U | L | D

adjacentCoords :: Direction -> Coord -> (Coord, Coord)
adjacentCoords R (C x y) = (C x y, C (x+1) y)
adjacentCoords U (C x y) = (C x y, C x (y+1))
adjacentCoords L (C x y) = (C x y, C (x-1) y)
adjacentCoords D (C x y) = (C x y, C x (y-1))

newPosition :: Direction -> Coord -> Coord
newPosition d c
  | isMovePossible (maze (snd movedCoord)) = snd movedCoord
  | otherwise = fst movedCoord
  where movedCoord = adjacentCoords d c

handleEvent :: Event -> (Direction, Coord) -> (Direction, Coord)
handleEvent (KeyPress key) (d, c)
    | key == "Right" = (R, newPosition R c)
    | key == "Up"    = (U, newPosition U c)
    | key == "Left"  = (L, newPosition L c)
    | key == "Down"  = (D, newPosition D c)
handleEvent _ x      = x

isMovePossible tile =
  case tile of
    Ground -> True
    Storage -> True
    _ -> False

rotate :: Direction -> Picture -> Picture
rotate d p =
  case d of
    R -> rotated (pi * (-0.5)) p
    U -> p
    L -> rotated (pi * 0.5) p
    D -> rotated pi p

atCoord :: Coord -> Picture -> Picture
atCoord (C x y) pic = translated (fromIntegral x) (fromIntegral y) pic

drawState (d, c) = atCoord c (rotate d player1) & pictureOfMaze

walk1 :: IO ()
walk1 = activityOf initialState handleEvent drawState

type Program = IO ()

program :: Program
program = walk1

main :: Program
main = program