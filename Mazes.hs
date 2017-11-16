module Mazes where

data Maze = Maze Coord (Coord -> Tile)

data Direction = R | U | L | D deriving (Eq, Show)
data Coord = C Integer Integer deriving (Eq, Show)
data Tile = Wall | Ground | Storage | Box | Blank deriving (Enum, Eq, Show)

maxRange = [-25..25]

mazes :: [Maze]
mazes = [maze1, maze2, maze3, maze4]

-- TODO
badMazes :: [Maze]
badMazes = [badMaze1, badMaze2, badMaze3, badMaze4]

allMazes :: [Maze]
allMazes = mazes ++ badMazes

maze1 :: Maze
maze1 = Maze start map where
  boxCoords = [(C (-1) 0), (C 0 0), (C 1 1)]
  storageCoords = [(C 0 1), (C 0 (-1)), (C 2 (-1))]
  start = C (-2) (-2)
  map coord@(C x y)
    | abs x > 3 || abs y > 3     = Blank
    | abs x > 2 || abs y > 2     = Wall
    | x == (-2) && y > 0         = Wall
    | x > 0 && elem y [(-2), 2]  = Wall
    | coord == C (-1) (-1)       = Wall
    | elem coord boxCoords       = Box
    | elem coord storageCoords   = Storage
    | otherwise                  = Ground

badMaze1 :: Maze
badMaze1 = Maze start newMap where
  Maze start map = maze1
  newWalls = [(C (-1) (-2)), (C (-2) (-1))]
  newMap coord
    | elem coord newWalls = Wall
    | otherwise           = map coord

maze2 :: Maze
maze2 = Maze start map where
  start = C (-3) 0
  map coord@(C x y)
    | abs x > 4 || abs y > 3       = Blank
    | abs x > 3 || abs y > 2       = Wall
    | x <= y - 4                   = Wall
    | x > 1 && y > 0               = Wall
    | y == -1 && elem x [-2, 1, 2] = Wall
    | y == 0  && elem x [-2..1]    = Box
    | x == -1 && elem y [-2..1]    = Storage
    | coord == C 1 (-2)            = Storage
    | otherwise                    = Ground

badMaze2 :: Maze
badMaze2 = Maze start newMap where
  Maze start map = maze2
  newWalls = [(C 0 (-1)), (C 0 (-2)), (C 3 (-1)), (C 3 (-2))]
  newMap coord
    | elem coord newWalls = Wall
    | otherwise           = map coord

maze3 :: Maze
maze3 = Maze start map where
  wallCoords = [(C (-2) (-1)), (C (-1) 1)]
  boxCoords = [(C 1 (-1)), (C 0 1), (C (-2) 1)]
  storageCoords = [(C (-1) (-2)), (C 0 0), (C (-2) 0)]
  start = C (-2) 2
  map coord@(C x y)
    | x > 3 || x < (-4) || abs y > 3 = Blank
    | x > 2 || x < (-3) || abs y > 2 = Wall
    | x == (-3) && y > 0             = Wall
    | x > 0 && (y > 0 || y == (-2))  = Wall
    | elem coord wallCoords          = Wall
    | elem coord boxCoords           = Box
    | elem coord storageCoords       = Storage
    | otherwise                      = Ground

badMaze3 :: Maze
badMaze3 = Maze start newMap where
  Maze start map = maze3
  newWalls = [(C (-1) 2), (C (-1) 0), (C (-3) 0)]
  newMap coord
    | elem coord newWalls = Wall
    | otherwise           = map coord

maze4 :: Maze
maze4 = Maze start map where
  boxCoords = [(C 0 0), (C 1 (-3))]
  storageCoords = [(C 0 (-3)), (C 0 2)]
  start = C (-2) (-3)
  map coord@(C x y)
    | x > 4 || x < (-3)         = Blank
    | y > 3 || y < (-4)         = Blank
    | x > 3 || x < (-2)         = Wall
    | y > 2 || y < (-3)         = Wall
    | x < 0 && y >= 0           = Wall
    | x > 2 && y <= 0           = Wall
    | coord == C 1 (-1)         = Wall
    | elem x [-1..1] && y == -2 = Box
    | elem coord boxCoords      = Box
    | x == 2 && elem y [-2..0]  = Storage
    | elem coord storageCoords  = Storage
    | otherwise                 = Ground

badMaze4 :: Maze
badMaze4 = Maze start newMap where
  Maze start map = maze4
  newMap (C x y)
    | elem x [0..3] && y == 1 = Wall
    | otherwise               = map (C x y)
