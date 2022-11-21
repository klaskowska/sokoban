{-# LANGUAGE OverloadedStrings #-}
import CodeWorld

startScreen :: Picture
startScreen = scaled 3 3 (lettering "Sokoban!")

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

data Direction = R | U | L | D

initialCoord :: Coord
initialCoord = C 0 1

data Position = P {dir :: Direction, coord :: Coord}

initialState2 :: Position
initialState2 = P U (C 0 1)

rotate :: Direction -> Picture -> Picture
rotate d p =
  case d of
    R -> rotated (pi * (-0.5)) p
    U -> p
    L -> rotated (pi * 0.5) p
    D -> rotated pi p

player1 :: Picture
player1 = polyline [(0, -0.4), (0, 0.4)] & polyline [(-0.2, 0.2), (0, 0.4), (0.2, 0.2)]

player2 :: Direction -> Picture
player2 d = rotate d player1

adjacentCoords :: Direction -> Coord -> (Coord, Coord)
adjacentCoords R (C x y) = (C x y, C (x+1) y)
adjacentCoords U (C x y) = (C x y, C x (y+1))
adjacentCoords L (C x y) = (C x y, C (x-1) y)
adjacentCoords D (C x y) = (C x y, C x (y-1))

isMovePossible tile =
  case tile of
    Ground -> True
    Storage -> True
    _ -> False

newPosition :: Direction -> Coord -> Coord
newPosition d c
  | isMovePossible (maze (snd movedCoord)) = snd movedCoord
  | otherwise = fst movedCoord
  where movedCoord = adjacentCoords d c

handleEvent1 :: Event -> Coord -> Coord
handleEvent1 (KeyPress key) c
    | key == "Right" = newPosition R c
    | key == "Up"    = newPosition U c
    | key == "Left"  = newPosition L c
    | key == "Down"  = newPosition D c
handleEvent1 _ c      = c

handleEvent2 :: Event -> Position -> Position
handleEvent2 (KeyPress key) pos
    | key == "Right" = go R
    | key == "Up"    = go U
    | key == "Left"  = go L
    | key == "Down"  = go D
    where go dir = P dir (newPosition dir (coord pos))
handleEvent2 _ x      = x

atCoord :: Coord -> Picture -> Picture
atCoord (C x y) pic = translated (fromIntegral x) (fromIntegral y) pic

drawState1 :: Coord -> Picture
drawState1 c = atCoord c player1 & pictureOfMaze

drawState2 :: Position -> Picture
drawState2 pos = atCoord (coord pos) (player2 (dir pos)) & pictureOfMaze

walk1 :: IO ()
walk1 = activityOf initialCoord handleEvent1 drawState1

isEscapePressed (KeyPress key)
  | key == "Esc" = True
isEscapePressed _ = False

isEscapeReleased (KeyRelease key)
  | key == "Esc" = True
isEscapeReleased _ = False

-- the game is reseted when `Esc` is pressed or released
resettableHandleEvent :: 
  world -> 
  (Event -> world -> world) -> 
  (Event -> world -> world)
resettableHandleEvent initialPos handleEvent = (\evn pos -> 
  if ((isEscapePressed evn) || (isEscapeReleased evn)) 
    then handleEvent evn initialPos 
  else handleEvent evn pos)

data SSState world = StartScreen | Running world

data Activity world = Activity {
  actState  :: world,
  actHandle :: (Event -> world -> world),
  actDraw   ::(world -> Picture)
}

resettable :: Activity s -> Activity s
resettable (Activity state0 handle draw)
  = Activity state0 handle' draw
  where handle' (KeyPress key) _ | key == "Esc" = state0
        handle' e s = handle e s

withStartScreen (Activity state0 handle draw)
  = Activity state0' handle' draw'
  where
    state0' = StartScreen

    handle' (KeyPress key) StartScreen
         | key == " "                  = Running state0
    handle' _              StartScreen = StartScreen
    handle' e              (Running s) = Running (handle e s)

    draw' StartScreen = startScreen
    draw' (Running s) = draw s

runActivity :: Activity s -> IO ()
runActivity (Activity state0 handle draw)
  = activityOf state0 handle draw

walk2 :: IO ()
walk2 = runActivity (Activity initialState2 handleEvent2 drawState2)

walk3 :: IO ()
walk3 = runActivity (resettable (Activity initialState2 handleEvent2 drawState2))

walk4 :: IO ()
walk4 = runActivity (resettable (withStartScreen (Activity initialState2 handleEvent2 drawState2)))

type Program = IO ()

program :: Program
program = walk3

main :: Program
main = program