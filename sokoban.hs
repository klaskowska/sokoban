{-# LANGUAGE OverloadedStrings #-}
import CodeWorld

startScreen :: Picture
startScreen = scaled 3 3 (lettering "Sokoban!")

winningScreen :: Picture
winningScreen = scaled 3 3 (lettering "You win!")

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

data Tile = Wall | Ground | Storage | Box | Blank deriving Eq

drawTile :: Tile -> Picture
drawTile Wall = wall
drawTile Ground = ground
drawTile Storage = storage
drawTile Box = box False
drawTile Blank = blankField

data Coord = C {x, y :: Integer}
instance Eq Coord where
  C x y == C x' y' = x == x' && y == y'

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

removeBoxes :: Maze -> Coord -> Tile
removeBoxes mazeDefinition = f . maze where f = removeBox

containsCoord :: Coord -> [Coord] -> Bool
containsCoord c coords = 
  case coords of
    [] -> False
    x:xs | c == x -> True
    _:xs -> containsCoord c xs

type Maze = Coord -> Tile
addBoxes :: [Coord] -> Maze -> Maze
addBoxes boxCoords mazeDefinition = (\c -> if containsCoord c boxCoords then Box else mazeDefinition c)

data Direction = R | U | L | D deriving Eq

initialCoord :: Coord
initialCoord = C 0 1

initialDirection :: Direction
initialDirection = U

initialBoxes :: Maze -> [Coord] -> [Coord]
initialBoxes mazeDefinition coords = filter (\c -> (mazeDefinition c) == Box) coords

data State = S {playerCoord :: Coord, playerDir :: Direction, boxCoords :: [Coord]}
instance Eq State where
  S playerCoord playerDir boxCoords == S playerCoord' playerDir' boxCoords' = playerCoord == playerCoord' && playerDir == playerDir' && boxCoords == boxCoords'

initialState :: Maze -> [Coord] -> State
initialState mazeDefinition coords = S initialCoord initialDirection (initialBoxes mazeDefinition coords)

rotate :: Direction -> Picture -> Picture
rotate d p =
  case d of
    R -> rotated (pi * (-0.5)) p
    U -> p
    L -> rotated (pi * 0.5) p
    D -> rotated pi p

playerFigure :: Picture
playerFigure = polyline [(0, -0.4), (0, 0.4)] & polyline [(-0.2, 0.2), (0, 0.4), (0.2, 0.2)]

player :: Direction -> Picture
player d = rotate d playerFigure

adjacentCoord :: Direction -> Coord -> Coord
adjacentCoord R (C x y) = C (x+1) y
adjacentCoord U (C x y) = C x (y+1)
adjacentCoord L (C x y) = C (x-1) y
adjacentCoord D (C x y) = C x (y-1)

currentTile :: Coord -> [Coord] -> Tile
currentTile coord boxCoords = 
  if containsCoord coord boxCoords then Box
    else
      case maze coord of
        Box -> Ground
        x -> x

moveBox :: [Coord] -> Coord -> Coord -> [Coord]
moveBox coords lastCoord newCoord = 
  (filter (\x -> x /= lastCoord) coords) ++ [newCoord]

updateStateWithMovingBox :: State -> Direction -> Coord -> State
updateStateWithMovingBox state dir newCoord = 
  case currentTile boxNewCoord (boxCoords state) of
    Ground -> movedBoxState
    Storage -> movedBoxState
    _ -> S (playerCoord state) dir (boxCoords state)
  where boxNewCoord = adjacentCoord dir newCoord
        movedBoxState = S newCoord dir (moveBox (boxCoords state) newCoord boxNewCoord)

updateState :: State -> Direction -> State
updateState state dir =
  case newPositionTile of
    Ground -> newStateWithoutMovingBox
    Storage -> newStateWithoutMovingBox
    Box -> updateStateWithMovingBox state dir newCoord
    _ -> S (playerCoord state) dir (boxCoords state)
    where newCoord = adjacentCoord dir (playerCoord state)
          newPositionTile = currentTile newCoord (boxCoords state)
          newStateWithoutMovingBox = S newCoord dir (boxCoords state)

allList :: [Bool] -> Bool
allList bools = 
  case bools of
    [] -> True
    x:xs | x == True -> allList xs
    _ -> False
    
isWinning :: State -> Bool
isWinning state = allList (map (\c -> (maze c) == Storage) (boxCoords state))

handleEvent :: Event -> State -> State
handleEvent _ state
    | isWinning state = state
handleEvent (KeyPress key) state
    | key == "Right" = go R
    | key == "Up"    = go U
    | key == "Left"  = go L
    | key == "Down"  = go D
    where go dir = updateState state dir 
handleEvent _ state      = state

atCoord :: Coord -> Picture -> Picture
atCoord (C x y) pic = translated (fromIntegral x) (fromIntegral y) pic

draw :: State -> Picture
draw state = 
  if isWinning state then winningScreen & board else board
  where board = atCoord (playerCoord state) (player (playerDir state)) 
                & pictureOfMaze (addBoxes (boxCoords state) (removeBoxes maze))

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
    
data WithUndo a = WithUndo a [a]

withUndo :: Eq a => Activity a -> Activity (WithUndo a)
withUndo (Activity state0 handle draw) = Activity state0' handle' draw' where
    state0' = WithUndo state0 []
    handle' (KeyPress key) (WithUndo s stack) | key == "U"
      = case stack of s':stack' -> WithUndo s' stack'
                      []        -> WithUndo s []
    handle' e              (WithUndo s stack)
       | s' == s = WithUndo s stack
       | otherwise = WithUndo (handle e s) (s:stack)
      where s' = handle e s
    draw' (WithUndo s _) = draw s

runActivity :: Activity s -> IO ()
runActivity (Activity state0 handle draw)
  = activityOf state0 handle draw

sokobanActivity :: Activity State
sokobanActivity = Activity (initialState maze (initialBoxes maze boardCoords)) handleEvent draw

walk :: IO ()
walk = runActivity (resettable (withStartScreen (withUndo sokobanActivity)))

type Program = IO ()

program :: Program
program = walk

main :: Program
main = program