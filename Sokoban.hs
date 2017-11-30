{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

import CodeWorld
import Data.Text (pack)
import Mazes
import Utils


data State = S Coord Integer Integer [Coord] deriving (Eq, Show)

data SSState world = StartScreen | Running world deriving Eq

data WithUndo a = WithUndo a [a]

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
box     = colored boxCol     (solidRectangle 0.8 0.8) & ground

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

drawTile :: Tile -> Picture
drawTile Wall    = wall
drawTile Ground  = ground
drawTile Storage = storage
drawTile Box     = box
drawTile Blank   = blank

tileFromMaze :: Maze -> Integer -> Integer -> Picture
tileFromMaze (Maze _ board) i j = translated (fromIntegral i) (fromIntegral j)
                                             (drawTile (board (C i j)))

pictureOfMaze :: Maze -> Picture
pictureOfMaze maze = pictures([tileFromMaze maze i j | i <- maxRange, j <- maxRange])

atState :: Coord -> Picture -> Picture
atState (C i j) p = translated (-(fromIntegral i)) (-(fromIntegral j)) p

player :: Picture
player = scaled 0.8 0.8 (eyes & mouth & hatball & hat & forhead & hands & shirt & boots & pants)


startScreen :: Picture
startScreen = scaled 3 3 (text "Sokoban")

finishScreen :: State -> Picture
finishScreen state
  | gameFinished    = translated 0 (-3) congrats
                    & translated 0 3 message
  | otherwise       = message
  where
    (S _ moves maze _) = state
    message = scaled 0.5 0.5 (text (pack messageText))
    messageText = "Poziom " ++ show maze ++ " ukończony, liczba ruchów: " ++ show moves
    gameFinished = maze == (listLength mazes)
    congrats = scaled 0.5 0.5 (text "Ukończyłeś wszystkie poziomy, brawo! Wciskając spację wrócisz do ostatniego poziomu")

removeBoxes :: Maze -> Maze
removeBoxes (Maze coord0 board) = (Maze coord0 newBoard)
  where
    newBoard = f . board where f = (\case Box -> Ground; x -> x)

addBoxes :: [Coord] -> Maze -> Maze
addBoxes boxes (Maze coord0 board) = (Maze coord0 newBoard)
  where
    newBoard x
      | elem x boxes = Box
      | otherwise    = board x

updatedMaze :: Maze -> [Coord] -> Maze
updatedMaze maze boxes = addBoxes boxes (removeBoxes maze)

mazeFromIndex :: Integer -> Maze
mazeFromIndex n = nthOrLast mazes n

boardFromIndex :: Integer -> Coord -> Tile
boardFromIndex n = board
  where
    Maze _ board = mazeFromIndex n

mazeFromState :: State -> Maze
mazeFromState (S _ _ maze boxes) = updatedMaze (mazeFromIndex maze) boxes

boardFromState :: State -> Coord -> Tile
boardFromState state = board
  where
    Maze _ board = mazeFromState state


replace :: Eq a => a -> a -> [a] -> [a]
replace _ _ [] = []
replace old new (x:xs)
  | old == x  = new : rest
  | otherwise = x   : rest
  where
    rest = replace old new xs

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

moveObject :: Direction -> Coord -> State -> State
moveObject dir objCoord state@(S playerCoord moves maze boxes)
  | spaceTaken = state
  | otherwise  = case oldTile of Box -> S playerCoord moves maze newBoxes
                                 _   -> S newCoord moves maze boxes
  where
    spaceTaken = not (freeCoord newTile)
    newCoord = adjacentCoord dir objCoord
    newBoxes = replace objCoord newCoord boxes
    oldTile = (boardFromState state) objCoord
    newTile = (boardFromState state) newCoord

changeStateAndMoveBox :: Direction -> State -> State
changeStateAndMoveBox dir (S coord moves maze boxes) =
  moveObject dir coord newState
  where
    newPlayerState = (S coord moves maze boxes)
    newCoord = adjacentCoord dir coord
    newState
      | elem newCoord boxes = moveObject dir newCoord newPlayerState
      | otherwise           = newPlayerState


initialBoxes :: Maze -> [Coord]
initialBoxes (Maze _ board) =
  [(C i j) | i <- maxRange, j <- maxRange, board (C i j) == Box]

initialState :: State
initialState = S coord0 0 initIndex (initialBoxes initMaze)
  where
    initIndex = 1
    initMaze = mazeFromIndex initIndex
    Maze coord0 _ = initMaze

handleTime :: Double -> State -> State
handleTime _ state = state

handleEvent :: Event -> State -> State
handleEvent (KeyPress key) (S coord moves maze boxes)
  | isWinning state =
    if key == " " then nextLevelState else state
  | key == "Right" = changeStateAndMoveBox R state
  | key == "Up"    = changeStateAndMoveBox U state
  | key == "Left"  = changeStateAndMoveBox L state
  | key == "Down"  = changeStateAndMoveBox D state
  where
    nextLevelState =
      let newMaze = mazeFromIndex (maze + 1)
          (Maze coord0 _) = newMaze
          newBoxes = initialBoxes newMaze
      in (S coord0 0 (maze + 1) newBoxes)
    state          = (S coord incMoves maze boxes)
    incMoves       = moves + 1
handleEvent _ s = s

boardNeighbours :: (Coord -> Tile) -> Coord -> [Coord]
boardNeighbours board (C x y) = filter (\x -> not (elem (board x) [Blank, Wall]))
[(C (x+1) y), (C x (y+1)), (C (x-1) y), (C x (y-1))]

allTiles :: Maze -> Tile -> [Coord]
allTiles (Maze coord0 board) tile =
filterGraph isTile coord0 neighbours doNotStop
where
neighbours = boardNeighbours board
isTile     = (\x -> board x == tile)
doNotStop  = (\_ -> False)

isWinning :: State -> Bool
isWinning (S _ _ maze boxes) = (not (null availableBoxes)) &&
 (and (map (\x -> (boardFromIndex maze) x == Storage) availableBoxes))
  where
    currMaze = updatedMaze(mazeFromIndex maze) boxes
    availableBoxes = allTiles currMaze Box

draw :: State -> Picture
draw state@(S coord _ _ _)
  | isWinning state = finishScreen state
  | otherwise       = world
  where
    world = player & (atState coord (pictureOfMaze (mazeFromState state)))


resettable :: Interaction s -> Interaction s
resettable (Interaction state0 step handle draw)
  = Interaction state0 step handle' draw
  where
    handle' (KeyPress key) _ | key == "Esc" = state0
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

withUndo :: (Eq s) => Interaction s -> Interaction (WithUndo s)
withUndo (Interaction state0 step handle draw) = Interaction state0' step' handle' draw'
  where
    state0' = WithUndo state0 []
    step' t (WithUndo s stack) = WithUndo (step t s) stack
    handle' (KeyPress key) (WithUndo s stack) | key == "U"
      = case stack of s':stack' -> WithUndo s' stack'
                      []        -> WithUndo s []
    handle' e              (WithUndo s stack)
       | s' == s   = WithUndo s stack
       | otherwise = WithUndo (handle e s) (s:stack)
      where
        s' = handle e s
    draw' (WithUndo s _) = draw s

runInteraction :: Interaction s -> IO ()
runInteraction (Interaction state0 step handle draw) =
  interactionOf state0 step handle draw

main :: IO ()
main = runInteraction $ resettable . withStartScreen . withUndo
       $ Interaction initialState handleTime handleEvent draw
