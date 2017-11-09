{-# LANGUAGE OverloadedStrings #-}

import CodeWorld

data Tile = Wall | Ground | Storage | Box | Blank deriving Eq

data Direction = R | U | L | D deriving Show

data Coord = C Integer Integer deriving (Eq, Show)

type Maze = Coord -> Tile

data SSState world = StartScreen | Running world

data Interaction world = Interaction
  world
  (Double -> world -> world)
  (Event -> world -> world)
  (world -> Picture)


wallCol, groundCol, storageCol, boxCol :: Color
wallCol    = RGBA 0.39 0.39 0.39 1
groundCol  = RGBA 0.89 0.8  0.45 1
storageCol = RGBA 1    0.39 0.39 1
boxCol     = RGBA 0.56 0.2  0    1

hatCol, forheadCol, handCol, shirtCol, pantsCol :: Color
hatCol     = RGBA 0.28 0.68 0.71 1
forheadCol = RGBA 0.95 0.82 0.67 1
handCol    = RGBA 0.93 0.82 0.01 1
shirtCol   = RGBA 0.64 0.17 0.11 1
pantsCol   = RGBA 0.30 0.25 0.15 1

wall, ground, storage, box :: Picture
wall    = colored wallCol    (solidRectangle 1 1)
ground  = colored groundCol  (solidRectangle 1 1)
storage = colored storageCol (solidCircle (0.3)) & ground
box     = colored boxCol     (solidRectangle 1 1)

eye, eyes, mouth, hatball, hat, forhead, hand, hands, shirt, boots, pants :: Picture
eye     = solidCircle 0.008 & colored white (solidCircle 0.08)
eyes    = translated (-0.08) 0.15 eye & translated 0.08 0.15 eye
mouth   = colored black (scaled 0.05 0.02 (arc pi (2*pi) 1))
hatball = translated 0 0.4 (colored handCol (scaled 0.09 0.03 (solidCircle 1)))
hat     = translated 0 0.25 (colored hatCol (scaled 0.32 0.15 (sector 0 pi 1)))
forhead = translated 0 0.15 (colored forheadCol (scaled 0.35 0.25 (solidCircle 1)))
hand    = colored handCol (scaled 0.06 0.08 (solidCircle 1))
hands   = translated (-0.42) (-0.15) hand & translated 0.42 (-0.15) hand
shirt   = translated 0 (-0.1) (colored shirtCol (scaled 0.45 0.25 (solidCircle 1)))
boots   = translated (-0.18) (-0.4) (scaled 0.18 0.05 (sector 0 pi 1))
        & translated 0.18 (-0.4) (scaled 0.18 0.05 (sector 0 pi 1))
pants   = colored pantsCol (solidPolygon [(-0.35, -0.4), (0.35, -0.4),
                                          (0.43, (-0.1)), (-0.43, (-0.1))])


maze :: Maze
maze (C x y)
  | abs x > 4  || abs y > 4  = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x ==  2 && y <= 0        = Wall
  | x ==  3 && y <= 0        = Storage
  | x >= -2 && y == 0        = Box
  | otherwise                = Ground

drawTile :: Tile -> Picture
drawTile Wall    = wall
drawTile Ground  = ground
drawTile Storage = storage
drawTile Box     = box
drawTile Blank   = blank

tileFromMaze :: Maze -> Integer -> Integer -> Picture
tileFromMaze maze i j = translated (fromIntegral i) (fromIntegral j)
                                   (drawTile (maze (C i j)))

pictureOfMaze :: Maze -> Picture
pictureOfMaze maze = pictures([tileFromMaze maze i j | i <- [-10..10], j <- [-10..10]])

playerPicture :: Picture
playerPicture = eyes & mouth & hatball & hat & forhead & hands & shirt & boots & pants

getRotation :: Direction -> Double
getRotation dir =
  case dir of
    R -> 3
    U -> 0
    L -> 1
    D -> 2

player :: Direction -> Picture
player dir = (rotated ((getRotation dir) * pi / 2) playerPicture)

atState :: Coord -> Picture -> Picture
atState (C i j) p = translated (-(fromIntegral i)) (-(fromIntegral j)) p


freeCoord :: Tile -> Bool
freeCoord t =
  case t of
    Ground  -> True
    Storage -> True
    _       -> False

adjacentCoord :: Direction -> Coord -> Coord
adjacentCoord dir (C i j) =
  case dir of
    R -> C (i+1) j
    U -> C  i   (j+1)
    L -> C (i-1) j
    D -> C  i   (j-1)

changeState :: Direction -> State -> State
changeState dir (S coord _ boxes) = S resultCoord dir boxes where
  newCoord  = adjacentCoord dir coord
  spaceFree = freeCoord ((mazeWrapper boxes) newCoord)
  resultCoord
    | spaceFree = newCoord
    | otherwise = coord


initialState :: State
initialState = S (C (-1) (-1)) U []

handleTime :: Double -> State -> State
handleTime _ p = p


startScreen :: Picture
startScreen = scaled 3 3 (text "Sokoban!")

resettable :: Interaction s -> Interaction s
resettable (Interaction state0 step handle draw)
  = Interaction state0 step handle' draw
  where handle' (KeyPress key) _ | key == "Esc" = state0
        handle' e s = handle e s

withStartScreen :: Interaction s -> Interaction (SSState s)
withStartScreen (Interaction state0 step handle draw)
  = Interaction state0' step' handle' draw'
  where
    state0' = StartScreen

    step' _ StartScreen = StartScreen
    step' t (Running s) = Running (step t s)

    handle' (KeyPress key) StartScreen
         | key == " "                  = Running state0
    handle' _              StartScreen = StartScreen
    handle' e              (Running s) = Running (handle e s)

    draw' StartScreen = startScreen
    draw' (Running s) = draw s


runInteraction :: Interaction s -> IO ()
runInteraction (Interaction state0 step handle draw) =
  interactionOf state0 step handle draw

stillBoxesInteraction :: Interaction State
stillBoxesInteraction =
  Interaction initialState handleTime handleEvent draw

walk3 :: IO ()
walk3 = runInteraction $ resettable $ stillBoxesInteraction

walk4 :: IO ()
walk4 = runInteraction $ resettable . withStartScreen $ stillBoxesInteraction


-- Stage 1
data State = S Coord Direction [Coord] deriving Show

-- Stage 2
initialBoxes :: [Coord]
initialBoxes = [(C i j) | i <- [-10..10], j <- [-10..10], maze (C i j) == Box]

initialStateWithBoxes :: State
initialStateWithBoxes = S (C (-1) (-1)) U initialBoxes

-- Stage 3
removeBoxes :: Maze -> Maze
removeBoxes maze = f . maze where f = (\x -> if x == Box then Ground; else x)

addBoxes :: [Coord] -> Maze -> Maze
addBoxes boxes maze x
  | elem x boxes = Box
  | otherwise    = maze x

-- Stage 4
drawBoxes :: [Coord] -> Picture
drawBoxes []           = blank
drawBoxes ((C i j):xs) = translated (fromIntegral i) (fromIntegral j) (drawTile Box)
                       & drawBoxes (xs)

finishScreen :: Picture
finishScreen = scaled 3 3 (text "Finished!")

draw :: State -> Picture
draw (S coord dir boxes)
  | isWinning (S coord dir boxes) = finishScreen & world
  | otherwise                     = world
  where world = (player dir)
              & (atState coord (pictureOfMaze (mazeWrapper boxes)))

-- Stage 5
updatedMaze :: [Coord] -> Maze
updatedMaze boxes = addBoxes boxes (removeBoxes maze)

mazeWrapper :: [Coord] -> Maze
mazeWrapper []    = maze
mazeWrapper boxes = updatedMaze boxes

replace :: Eq a => a -> a -> [a] -> [a]
replace _ _ [] = []
replace old new (x:xs)
  | old == x  = new : rest
  | otherwise = x   : rest
  where rest = replace old new xs

moveBox :: Direction -> Coord -> State -> State
moveBox dir boxCoord (S playerCoord _ boxes) = (S playerCoord dir newBoxes) where
  newCoord = adjacentCoord dir boxCoord
  newBoxes
    | freeCoord ((mazeWrapper boxes) newCoord) = replace boxCoord newCoord boxes
    | otherwise                                = boxes

changeStateAndMoveBox :: Direction -> State -> State
changeStateAndMoveBox dir (S coord _ boxes) = changeState dir newState where
  newCoord = adjacentCoord dir coord
  newState
    | elem newCoord boxes = moveBox dir newCoord (S coord dir boxes)
    | otherwise           = (S coord dir boxes)

handleEvent :: Event -> State -> State
handleEvent (KeyPress key) p
    | key == "Right" = changeStateAndMoveBox R p
    | key == "Up"    = changeStateAndMoveBox U p
    | key == "Left"  = changeStateAndMoveBox L p
    | key == "Down"  = changeStateAndMoveBox D p
handleEvent _ p      = p

-- Stage 6
main :: IO ()
main = runInteraction $ resettable . withStartScreen
       $ Interaction initialStateWithBoxes handleTime handleEvent draw

-- Stage 7
isWinning :: State -> Bool
isWinning (S _ _ boxes) = (not (null boxes)) &&
   (and (map (\x -> maze x == Storage) boxes))
