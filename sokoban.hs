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

equalCoord :: Coord -> Coord -> Bool
equalCoord (C x1 y1) (C x2 y2) = x1 == x2 && y1 == y2

boardCoords :: [Coord]
boardCoords = [C x y | x <- [-10 .. 10], y <- [-10 .. 10]]

maze :: Coord -> Tile
maze c
  | abs (x c) > 4  || abs (y c) > 4 = Blank
  | abs (x c) == 4 || abs (y c) == 4 = Wall
  | (x c) ==  2 && (y c) <= 0 = Wall
  | (x c) ==  3 && (y c) <= 0 = Storage
  | (x c) >= -2 && (y c) == 0 = Box
  | otherwise = Ground

pictureOfMaze :: Maze -> Picture
pictureOfMaze mazeDefinition = pictures [translated (fromIntegral x) (fromIntegral y) (drawTile (mazeDefinition (C x y))) 
  | x <- [-10 .. 10], y <- [-10 .. 10]]

removeBox t = 
  case t of
    Box -> Ground
    x -> x

removeBoxes :: (Coord -> Tile) -> Coord -> Tile
removeBoxes mazeDefinition = f . maze where f = removeBox

containsCoord :: Coord -> [Coord] -> Bool
containsCoord c coords = 
  case coords of
    [] -> False
    x:xs | equalCoord c x -> True
    _:xs -> containsCoord c xs

type Maze = Coord -> Tile
addBoxes :: [Coord] -> Maze -> Maze
addBoxes boxCoords mazeDefinition = (\c -> if containsCoord c boxCoords then Box else mazeDefinition c)

data Direction = R | U | L | D

initialCoord :: Coord
initialCoord = C 0 1

initialDirection :: Direction
initialDirection = U

initialBoxes :: (Coord -> Tile) -> [Coord] -> [Coord]
initialBoxes mazeDefinition coords = filter (\c -> isBox (mazeDefinition c)) coords

data State = S {playerCoord :: Coord, playerDir :: Direction, boxCoords :: [Coord]}

initialState :: Maze -> [Coord] -> State
initialState mazeDefinition coords = S initialCoord initialDirection (initialBoxes mazeDefinition coords)

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

newCoord :: Direction -> Coord -> Coord
newCoord d c
  | isMovePossible (maze (snd movedCoord)) = snd movedCoord
  | otherwise = fst movedCoord
  where movedCoord = adjacentCoords d c

currentTile :: Coord -> [Coord] -> Tile
currentTile coord boxCoords = 
  if containsCoord coord boxCoords then Box
    else
      case maze coord of
        Box -> Ground
        x -> x

moveBox :: [Coord] -> Coord -> Coord -> [Coord]
moveBox coords lastCoord newCoord = 
  (filter (\x -> not (equalCoord x lastCoord)) coords) ++ [newCoord]

updateStateWithMovingBox :: State -> Direction -> Coord -> State
updateStateWithMovingBox state dir newCoord = 
  case currentTile boxNewCoord (boxCoords state) of
    Ground -> movedBoxState
    Storage -> movedBoxState
    _ -> S (playerCoord state) dir (boxCoords state)
  where boxNewCoord = snd (adjacentCoords dir newCoord)
        movedBoxState = S newCoord dir (moveBox (boxCoords state) newCoord boxNewCoord)

updateState :: State -> Direction -> State
updateState state dir =
  case newPositionTile of
    Ground -> newStateWithoutMovingBox
    Storage -> newStateWithoutMovingBox
    Box -> updateStateWithMovingBox state dir newCoord
    _ -> S (playerCoord state) dir (boxCoords state)
    where newCoord = snd (adjacentCoords dir (playerCoord state))
          newPositionTile = currentTile newCoord (boxCoords state)
          newStateWithoutMovingBox = S newCoord dir (boxCoords state)

handleEvent :: Event -> State -> State
handleEvent (KeyPress key) state
    | key == "Right" = go R
    | key == "Up"    = go U
    | key == "Left"  = go L
    | key == "Down"  = go D
    where go dir = updateState state dir 
handleEvent _ state      = state

atCoord :: Coord -> Picture -> Picture
atCoord (C x y) pic = translated (fromIntegral x) (fromIntegral y) pic

drawState1 :: Coord -> Picture
drawState1 c = atCoord c player1 & pictureOfMaze maze

draw :: State -> Picture
draw (S playerCoord playerDir boxCoords) = atCoord playerCoord (player2 playerDir)
 & pictureOfMaze (addBoxes boxCoords (removeBoxes maze))

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

isBox :: Tile -> Bool
isBox t =
  case t of
    Box -> True
    _ -> False

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

walk4 :: IO ()
walk4 = runActivity (resettable (withStartScreen (Activity (initialState maze (initialBoxes maze boardCoords)) handleEvent draw)))

type Program = IO ()

program :: Program
program = walk4

main :: Program
main = program