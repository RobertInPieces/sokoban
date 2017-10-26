{-# LANGUAGE OverloadedStrings #-}

import Data.Text
import CodeWorld

data Tile = Wall | Ground | Storage | Box | Blank
data Direction = R | U | L | D deriving Show
data Coord = C Integer Integer deriving Show

handCol, hatCol, forheadCol, shirtCol, pantsCol :: Color
hatCol     = RGBA 0.28 0.68 0.71 1
forheadCol = RGBA 0.95 0.82 0.67 1
handCol    = RGBA 0.93 0.82 0.01 1
shirtCol   = RGBA 0.64 0.17 0.11 1
pantsCol   = RGBA 0.30 0.25 0.15 1

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
pants   = colored pantsCol (solidPolygon [(-0.35, -0.4), (0.35, -0.4), (0.43, (-0.1)), (-0.43, (-0.1))])

player1 :: Picture
player1 = eyes & mouth & hatball & hat & forhead & hands & shirt & boots & pants

wallCol, groundCol, storageCol, boxCol, emptyCol :: Color
wallCol    = RGBA 0.39 0.39 0.39 1
groundCol  = RGBA 0.89 0.8  0.45 1
storageCol = RGBA 1    0.39 0.39 1
boxCol     = RGBA 0.56 0.2  0    1
emptyCol   = white

wall, ground, storage, box, empty :: Picture
wall    = colored wallCol    (solidRectangle 1 1)
ground  = colored groundCol  (solidRectangle 1 1)
storage = colored storageCol (solidCircle (0.3)) & ground
box     = colored boxCol     (solidRectangle 1 1)
empty   = colored emptyCol   (solidRectangle 1 1)

drawTile :: Tile -> Picture
drawTile Wall    = wall
drawTile Ground  = ground
drawTile Storage = storage
drawTile Box     = box
drawTile Blank   = blank

maze :: Coord -> Tile
maze (C x y)
  | abs x > 4  || abs y > 4  = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x ==  2 && y <= 0        = Wall
  | x ==  3 && y <= 0        = Storage
  | x >= -2 && y == 0        = Box
  | otherwise                = Ground

drawTileFromMaze :: Integer -> Integer -> Picture
drawTileFromMaze i j = translated (fromIntegral i) (fromIntegral j)
                                  (drawTile (maze (C i j)))

pictureOfMaze :: Picture
pictureOfMaze = pictures([drawTileFromMaze i j | i <- [-10..10],
                                                 j <- [-10..10]])

initialCoord :: Coord
initialCoord = C (-1) (-1)

atCoord :: Coord -> Picture -> Picture
atCoord (C i j) p = translated (fromIntegral i) (fromIntegral j) p

adjacentCoord :: Direction -> Coord -> Coord
adjacentCoord R (C i j) = C (i+1) j
adjacentCoord U (C i j) = C  i   (j+1)
adjacentCoord L (C i j) = C (i-1) j
adjacentCoord D (C i j) = C  i   (j-1)

changeCoord :: Direction -> Coord -> Coord
changeCoord d (C i j)
  | freeCoord (maze newC) = newC
  | otherwise             = (C i j)
  where newC = adjacentCoord d (C i j)

freeCoord :: Tile -> Bool
freeCoord t =
  case t of
    Ground  -> True
    Storage -> True
    _       -> False

moveCoords :: [Direction] -> Coord -> Coord
moveCoords [] (C i j) = C i j
moveCoords (x:xs) (C i j) = moveCoords xs (adjacentCoord x (C i j))

handleTime :: Double -> Coord -> Coord
handleTime _ c = c

handleEvent :: Event -> Coord -> Coord
handleEvent (KeyPress key) c
    | key == "Right" = changeCoord R c
    | key == "Up"    = changeCoord U c
    | key == "Left"  = changeCoord L c
    | key == "Down"  = changeCoord D c
handleEvent _ c      = c

drawState :: Coord -> Picture
drawState c = atCoord c player1 & pictureOfMaze

walk1 :: IO ()
walk1 = interactionOf initialCoord handleTime handleEvent drawState

data Placement = P Coord Direction deriving Show

getRotation :: Direction -> Double
getRotation d =
  case d of
    R -> 3
    U -> 0
    L -> 1
    D -> 2

mazeWrapper :: Placement -> Tile
mazeWrapper (P (C i j) d) = maze (C i j)

atPlacement :: Placement -> Picture -> Picture
atPlacement (P (C i j) d) p = translated (fromIntegral i) (fromIntegral j)
                                      (rotated ((getRotation d) * pi / 2) p)

adjacentPlacement :: Direction -> Placement -> Placement
adjacentPlacement R (P (C i j) _) = P (C (i+1) j)    R
adjacentPlacement U (P (C i j) _) = P (C  i   (j+1)) U
adjacentPlacement L (P (C i j) _) = P (C (i-1) j)    L
adjacentPlacement D (P (C i j) _) = P (C  i   (j-1)) D

changePlacement :: Direction -> Placement -> Placement
changePlacement d (P (C i j) _)
  | freeCoord (mazeWrapper newC) = newC
  | otherwise             = (P (C i j) d)
  where newC = adjacentPlacement d (P (C i j) d)

initialPlacement :: Placement
initialPlacement = P (C (-1) (-1)) U

handleTime2 :: Double -> Placement -> Placement
handleTime2 _ p = p

handleEvent2 :: Event -> Placement -> Placement
handleEvent2 (KeyPress key) p
    | key == "Right" = changePlacement R p
    | key == "Up"    = changePlacement U p
    | key == "Left"  = changePlacement L p
    | key == "Down"  = changePlacement D p
handleEvent2 _ p      = p

drawState2 :: Placement -> Picture
drawState2 p = atPlacement p player1 & pictureOfMaze

walk2 :: IO ()
walk2 = interactionOf initialPlacement handleTime2 handleEvent2 drawState2

-- KeyRelease "Esc" doesn't seem to matter that much, but since we're handling
-- KeyPresses it would be inappropriate to ignore KeyRelease (leading to bugs,
-- somebody mistakenly using resettableInteractionOf and programming Esc on his
-- own handler should see both of them not working, leaving one alone could be
-- a cause of some weird interactions) so we should just ignore release of it
handleEventOrReset :: world -> (Event -> world -> world) ->
                      Event -> world -> world
handleEventOrReset w f e c
    | e == KeyPress "Esc"   = w
    | e == KeyRelease "Esc" = c
    | otherwise             = f e c

resettableInteractionOf ::
    world ->
    (Double -> world -> world) ->
    (Event -> world -> world) ->
    (world -> Picture) ->
    IO ()
resettableInteractionOf w t e p = interactionOf w t (handleEventOrReset w e) p

walk3 :: IO ()
walk3 = resettableInteractionOf initialPlacement handleTime2 handleEvent2 drawState2
