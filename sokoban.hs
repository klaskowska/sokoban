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
  
drawTile :: Int -> Picture
drawTile fieldType
  | fieldType == 1 = wall
  | fieldType == 2 = ground
  | fieldType == 3 = storage
  | fieldType == 4 = box False
  | otherwise = blankField

maze :: Int -> Int -> Int
maze x y
  | abs x > 4  || abs y > 4  = 0  -- blank
  | abs x == 4 || abs y == 4 = 1  -- wall
  | x ==  2 && y <= 0        = 1  -- wall
  | x ==  3 && y <= 0        = 3  -- storage
  | x >= -2 && y == 0        = 4  -- box
  | otherwise                = 2  -- ground

pictureOfMaze :: Picture
pictureOfMaze = pictures [translated (fromIntegral x) (fromIntegral y) (drawTile (maze x y)) 
  | x <- [-10 .. 10], y <- [-10 .. 10]]

type Program = IO ()

program :: Program
program = drawingOf pictureOfMaze

main :: Program
main = program