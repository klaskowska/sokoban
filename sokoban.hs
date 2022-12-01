{-# LANGUAGE OverloadedStrings #-}
import CodeWorld
import Data.String

startScreen :: Picture
startScreen = scaled 3 3 (lettering "Sokoban!")

picture :: (Show a) => a -> Picture
picture = lettering . fromString . show

winningScreen :: Integer -> Picture
winningScreen moveCounter = picture ("Poziom ukonczony, liczba ruchow:" ++ show moveCounter)

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

-- List and graph functions

foldList :: (a -> b -> b) -> b -> [a] -> b
foldList _ res [] = res
foldList fun res (x:xs) = foldList fun (fun x res) xs

reverseList :: [a] -> [a]
reverseList list = foldList (\e res -> e:res) [] list

elemList :: Eq a => a -> [a] -> Bool
elemList x list = foldList (\e res -> if res then res else e == x) False list

appendList :: [a] -> [a] -> [a]
appendList list1 list2 = foldList (\e res -> e:res) list2 (reverseList list1)

listLength :: [a] -> Integer
listLength list = foldList (\_ res -> res + 1) 0 list

filterList :: (a -> Bool) -> [a] -> [a]
filterList predicate list = reverseList (foldList (\e res -> if predicate e then e:res else res) [] list)

nth :: [a] -> Integer -> a
nth list n = fst (foldList (\e (res, i) -> if i == n then (e, i + 1) else (res, i + 1)) (error "Incorrect index", 0) list)

mapList :: (a -> b) -> [a] -> [b]
mapList fun list = reverseList (foldList (\e res -> (fun e):res) [] list)

andList :: [Bool] -> Bool
andList list = foldList (\e res -> if res then e else res) True list

allList :: (a -> Bool) -> [a] -> Bool
allList condition list = foldList (\e res -> if res then condition e else res) True list

isGraphClosedHelper :: Eq a => a -> (a -> [a]) -> (a -> Bool) -> [a] -> (Bool, [a])
isGraphClosedHelper initial neighbours isOk visited = if elemList initial visited 
  then (True, visited)
  else if isOk initial
    then foldList (\e (resIsOk, resVisited) -> 
      if resIsOk
        then let (eIsOk, eVisited) = isGraphClosedHelper e neighbours isOk resVisited in 
        if eIsOk then (True, e:eVisited) else (False, eVisited)
      else (resIsOk, resVisited)
    ) (True, initial:visited) (neighbours initial)
    else (False, visited)

isGraphClosed :: Eq a => a -> (a -> [a]) -> (a -> Bool) -> Bool
isGraphClosed initial neighbours isOk = fst (isGraphClosedHelper initial neighbours isOk [])

reachable :: Eq a => a -> a -> (a -> [a]) -> Bool
reachable v initial neighbours = not (isGraphClosed initial neighbours (\e -> e /= v))

allReachable :: Eq a => [a] -> a -> (a -> [a]) -> Bool
allReachable vs initial neighbours = allList (\e -> reachable e initial neighbours) vs

-- Mazes

data Maze = Maze {initialCoord :: Coord, mazeDef :: (Coord -> Tile)}
mazes :: [Maze]
mazes = [
  Maze (C (-2) (-2)) easy_testMaze_GN,
  Maze (C 1 (-1))    easy_spiralMaze_DM,
  Maze (C 0 0)       easy_decoratedMaze_BS,
  Maze (C 1 1)       medium_maze4_GN,
  Maze (C 0 1)       medium_maze3_GN,
  Maze (C 1 (-3))    hard_maze2_GN
  ]

easy_testMaze_GN :: Coord -> Tile
easy_testMaze_GN (C x y)
  | abs x > 4  || abs y > 4  = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x ==  2 && y <= 0        = Wall
  | x ==  3 && y <= 0        = Storage
  | x >= -2 && y == 0        = Box
  | otherwise                = Ground

hard_maze2_GN :: Coord -> Tile
hard_maze2_GN (C x y)
  | abs x > 4  || abs y > 4      = Blank
  | abs x == 4 || abs y == 4     = Wall
  | x ==  2 && y <   0           = Wall
  | x >= -1 && y ==  1 && x <= 2 = Wall
  | x == -3 && y ==  1           = Wall
  | x ==  0 && y ==  3           = Wall
  | x ==  0 && y ==  0           = Wall
  | x ==  3 && y == -3           = Storage
  | x ==  1 && y ==  2           = Storage
  | x == -3 && y ==  2           = Storage
  | x ==  1 && y == -1           = Storage
  | x == -2 && y ==  1           = Box
  | x ==  2 && y ==  2           = Box
  | x <=  1 && y == -2 && x >= 0 = Box
  | otherwise                    = Ground

medium_maze3_GN :: Coord -> Tile
medium_maze3_GN (C x y)
  | abs x >  4 || abs y >  4           = Blank
  | abs x == 4 || abs y == 4           = Wall
  | x ==     1 && y <      0           = Wall
  | x ==    -3 && y ==    -2           = Wall
  | x <=     1 && x >     -2 && y == 0 = Wall
  | x >     -3 && x <      3 && y == 2 = Wall
  | x ==     3 && y >      1           = Storage
  | y ==    -2 && x <      0           = Box
  | y ==    -2 && x ==     2           = Box
  | y ==    0  && x ==     3           = Box
  | y == -1    && x > 1      && x < 4  = Storage
  | otherwise                          = Ground

medium_maze4_GN :: Coord -> Tile
medium_maze4_GN (C x y)
  | abs x > 4  || abs y > 4                  = Blank
  | abs x == 4 || abs y == 4 || x == -3      = Wall
  | x == -2 && (y == 3 || y == 0)            = Wall
  | x == -1 &&  y == -1                      = Wall
  | x == -0 &&  y == 1                       = Wall
  | x ==  3 &&  y == 0                       = Wall
  | x <   0 && (y == 2 || y == -3)           = Storage
  | x == -1 &&  y == 1                       = Storage
  | x ==  0 && (y == 2 || y == 0 || y == -1) = Box
  | x ==  1 &&  y == -2                      = Box
  | x ==  2 &&  y == -3                      = Box
  | otherwise                                = Ground

easy_spiralMaze_DM :: Coord -> Tile
easy_spiralMaze_DM (C x y)
  | abs x >  4 || abs y > 4      = Blank
  | abs x == 4                   = Wall
  | abs y == 4                   = Wall
  | x ==  2 && y <=  1           = Wall
  | x >= -1 && x <=  2 && y == 1 = Wall
  | x == -1 && y >= -1 && y <= 1 = Wall
  | x ==  0 && y == -1           = Box
  | x ==  3 && y == -3           = Storage
  | otherwise                    = Ground

easy_decoratedMaze_BS :: Coord -> Tile
easy_decoratedMaze_BS (C x y)
  | abs x > 6  || abs y > 4          = Blank
  | abs x == 6 || abs y == 4         = Wall
  | abs x + abs y > 6                = Box
  | abs x == 4 || abs y == 2         = Wall
  | elem (x, y) [(-3, 1), (3, -1)]   = Box
  | x == 0     && abs y == 1         = Box
  | elem (x, y) [(-3, -1), (3, 1)]   = Storage
  | elem (x, y) [(-2, 1), (2, -1)]   = Wall
  | abs x > 1 && abs x < 4 && y == 0 = Wall
  | otherwise                        = Ground

badMazes :: [Maze]
badMazes = [
  Maze (C (-2) (-2)) badTestMaze_BS,
  Maze (C 1 (-1))    cutOffStorageMaze_DM,
  Maze (C (-1) 0)    holeInTheWallMaze_BS
  ]

badTestMaze_BS :: Coord -> Tile
badTestMaze_BS (C x y)
  | abs x > 4  || abs y > 4  = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x ==  2                  = Wall
  | x ==  3 && y <= 0        = Storage
  | x >= -2 && y == 0        = Box
  | otherwise                = Ground

cutOffStorageMaze_DM :: Coord -> Tile
cutOffStorageMaze_DM (C x y)
  | abs x > 7 || abs y > 7                            = Blank
  | abs x == 7                                        = Wall
  | abs y == 7                                        = Wall
  | x >= 4 && y == 4                                  = Wall
  | x == 4 && y >= 4                                  = Wall
  | x >= 5 && y >= 5                                  = Storage
  | elem (x, y) [(-6, 6), (-6, -6), (6, -6), (6, -5)] = Storage
  | x == 0 && elem y [-4 .. 2]                        = Box
  | otherwise                                         = Ground

holeInTheWallMaze_BS :: Coord -> Tile
holeInTheWallMaze_BS (C x y)
  | abs x > 2 || abs y > 1   = Blank
  | x == -2 && y == 0        = Ground
  | abs x == 2 || abs y == 1 = Wall
  | x == 0 && y == 0         = Box
  | x == 1 && y == 0         = Storage
  | otherwise                = Ground


pictureOfMaze :: (Coord -> Tile) -> Picture
pictureOfMaze mazeDef = pictures [translated (fromIntegral x) (fromIntegral y) (drawTile (mazeDef (C x y))) 
  | x <- [-10 .. 10], y <- [-10 .. 10]]

removeBox :: Tile -> Tile
removeBox t = 
  case t of
    Box -> Ground
    x -> x

removeBoxes :: (Coord -> Tile) -> (Coord -> Tile)
removeBoxes mazeDef = f . mazeDef where f = removeBox

containsCoord :: Coord -> [Coord] -> Bool
containsCoord c coords = 
  case coords of
    [] -> False
    x:xs | c == x -> True
    _:xs -> containsCoord c xs

addBoxes :: [Coord] -> (Coord -> Tile) -> (Coord -> Tile)
addBoxes boxCoords mazeDef = (\c -> if containsCoord c boxCoords then Box else mazeDef c)

data Direction = R | U | L | D deriving Eq

initialDirection :: Direction
initialDirection = U

initialBoxes :: (Coord -> Tile) -> [Coord]
initialBoxes mazeDef = filterList (\c -> (mazeDef c) == Box) boardCoords

data State = S {playerCoord :: Coord, playerDir :: Direction, boxCoords :: [Coord], mazeId :: Integer, moveCounter :: Integer}
instance Eq State where
  S playerCoord playerDir boxCoords mazeId moveCounter == S playerCoord' playerDir' boxCoords' mazeId' moveCounter' = 
    playerCoord == playerCoord' && playerDir == playerDir' && boxCoords == boxCoords' && mazeId == mazeId' && moveCounter == moveCounter'

initialState :: Integer -> State
initialState mazeId = S (initialCoord maze) initialDirection (initialBoxes (mazeDef maze)) mazeId 0
                      where maze = nth mazes mazeId

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

currentTile :: Coord -> [Coord] -> (Coord -> Tile) -> Tile
currentTile coord boxCoords mazeDef = 
  if containsCoord coord boxCoords then Box
    else
      case mazeDef coord of
        Box -> Ground
        x -> x

moveBox :: [Coord] -> Coord -> Coord -> [Coord]
moveBox coords lastCoord newCoord = 
  (filterList (\x -> x /= lastCoord) coords) ++ [newCoord]

updateStateWithMovingBox :: State -> Direction -> Coord -> State
updateStateWithMovingBox state dir newCoord = 
  case currentTile boxNewCoord (boxCoords state) (mazeDef ((nth mazes (mazeId state)))) of
    Ground -> movedBoxState
    Storage -> movedBoxState
    _ -> S (playerCoord state) dir (boxCoords state) (mazeId state) (moveCounter state)
  where boxNewCoord = adjacentCoord dir newCoord
        movedBoxState = S newCoord dir (moveBox (boxCoords state) newCoord boxNewCoord) (mazeId state) (moveCounter state)

updateState :: State -> Direction -> State
updateState state dir =
  case newPositionTile of
    Ground -> newStateWithoutMovingBox
    Storage -> newStateWithoutMovingBox
    Box -> updateStateWithMovingBox state dir newCoord
    _ -> S (playerCoord state) dir (boxCoords state) (mazeId state) (moveCounter state + 1)
    where newCoord = adjacentCoord dir (playerCoord state)
          newPositionTile = currentTile newCoord (boxCoords state) (mazeDef ((nth mazes (mazeId state))))
          newStateWithoutMovingBox = S newCoord dir (boxCoords state) (mazeId state) (moveCounter state + 1)

a :: Coord -> State -> Bool
a c state = (mazeDef (nth mazes (mazeId state))) c == Storage

isWinning :: State -> Bool
isWinning state = allList (\c -> ((mazeDef (nth mazes (mazeId state))) c) == Storage) (boxCoords state)

nextId :: State -> Integer
nextId state = if currentId < listLength mazes - 1 then currentId + 1 else currentId
               where currentId = mazeId state

handleEvent :: Event -> State -> State
handleEvent (KeyPress key) state
    | key == "N" = initialState (nextId state)
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
  if isWinning state then (winningScreen (moveCounter state)) & board else board
  where board = atCoord (playerCoord state) (player (playerDir state)) 
                & pictureOfMaze (addBoxes (boxCoords state) (removeBoxes (mazeDef (nth mazes (mazeId state)))))

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
sokobanActivity = Activity (initialState 0) handleEvent draw

walk :: IO ()
walk = runActivity (resettable (withStartScreen (withUndo sokobanActivity)))

type Program = IO ()

etap5 :: Program
etap5 = walk

main :: Program
main = etap5