{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

import Data.List (intercalate)
import System.IO

import Mazes
import Utils


data State = S Coord Integer Integer [Coord] deriving (Eq, Show)
data Event = KeyPress String
type Screen = String

data Interaction world = Interaction
    world
    (Event -> world -> world)
    (world -> Screen)

data SSState world = StartScreen | Running world deriving Eq

data WithUndo a = WithUndo a [a]

drawTile :: Tile -> Char
drawTile Player  = '@'
drawTile Wall    = '#'
drawTile Ground  = ' '
drawTile Storage = '.'
drawTile Box     = '*'
drawTile Blank   = ' '

screenOfMaze :: State -> Screen
screenOfMaze state@(S coord moves maze boxes) =
  intercalate "\n" [[drawTile (boardFromState state (C col row))
                      | col <- colsRange] | row <- rowsRange]

startScreen :: Screen
startScreen = "Sokoban"

finishScreen :: State -> Screen
finishScreen state
  | gameFinished    = congrats ++ "\n" ++ message
  | otherwise       = message
  where
    (S _ moves maze _) = state
    gameFinished = maze == (listLength mazes)
    message = "Poziom " ++ show maze ++ " ukończony, liczba ruchów: " ++ show moves
    congrats = "Ukończyłeś wszystkie poziomy, brawo! Wciskając spację wrócisz do ostatniego poziomu"

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

addPlayer :: Coord -> Maze -> Maze
addPlayer coord (Maze coord0 board) = (Maze coord0 newBoard)
  where
    newBoard x
      | x == coord = Player
      | otherwise  = board x

updatedMaze :: Maze -> Coord -> [Coord] -> Maze
updatedMaze maze coord boxes =
  addPlayer coord (addBoxes boxes (removeBoxes maze))

mazeFromIndex :: Integer -> Maze
mazeFromIndex n = nthOrLast mazes n

boardFromIndex :: Integer -> Coord -> Tile
boardFromIndex n = board
  where
    Maze _ board = mazeFromIndex n

mazeFromState :: State -> Maze
mazeFromState (S coord _ maze boxes) =
  updatedMaze (mazeFromIndex maze) coord boxes

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
adjacentCoord dir (C col row) =
  case dir of
    R -> C (col+1) row
    U -> C  col   (row+1)
    L -> C (col-1) row
    D -> C  col   (row-1)

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
changeStateAndMoveBox dir state@(S coord moves maze boxes) =
  moveObject dir coord newState
  where
    newCoord = adjacentCoord dir coord
    newState
      | elem newCoord boxes = moveObject dir newCoord state
      | otherwise           = state


initialBoxes :: Maze -> [Coord]
initialBoxes (Maze _ board) =
  [(C col row) | col <- colsRange, row <- rowsRange, board (C col row) == Box]

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
  | elem key ["C", "d"] = changeStateAndMoveBox R state
  | elem key ["A", "w"] = changeStateAndMoveBox U state
  | elem key ["D", "a"] = changeStateAndMoveBox L state
  | elem key ["B", "s"] = changeStateAndMoveBox D state
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
isWinning state@(S _ _ maze boxes) = (not (null availableBoxes)) &&
 (and (map (\x -> (boardFromIndex maze) x == Storage) availableBoxes))
  where
    availableBoxes = allTiles (mazeFromState state) Box

draw :: State -> Screen
draw state@(S coord _ _ _)
  | isWinning state = finishScreen state
  | otherwise       = world
  where
    world = screenOfMaze state


resettable :: Interaction s -> Interaction s
resettable (Interaction state0 handle draw)
  = Interaction state0 handle' draw
  where
    handle' (KeyPress key) _ | key == "r" = state0
    handle' e s = handle e s

withStartScreen :: Interaction s -> Interaction (SSState s)
withStartScreen (Interaction state0 handle draw)
  = Interaction state0' handle' draw'
  where
    state0' = StartScreen
    handle' (KeyPress key) StartScreen
         | key == " "                  = Running state0
    handle' _              StartScreen = StartScreen
    handle' e              (Running s) = Running (handle e s)
    draw' StartScreen = startScreen
    draw' (Running s) = draw s

withUndo :: (Eq s) => Interaction s -> Interaction (WithUndo s)
withUndo (Interaction state0 handle draw) = Interaction state0' handle' draw'
  where
    state0' = WithUndo state0 []
    handle' (KeyPress key) (WithUndo s stack) | key == "u"
      = case stack of s':stack' -> WithUndo s' stack'
                      []        -> WithUndo s []
    handle' e              (WithUndo s stack)
       | s' == s   = WithUndo s stack
       | otherwise = WithUndo (handle e s) (s:stack)
      where
        s' = handle e s
    draw' (WithUndo s _) = draw s

runInteraction :: Interaction s -> IO ()
runInteraction (Interaction state0 handle draw) = do
  hSetBuffering stdin NoBuffering
  initial
  input <- getContents
  go state0 input
    where
      initial       = putStr "\ESCc" >> putStrLn (draw state0)
      go _ []       = putStr "\nEOF\n"
      go s ('q':_)  = putStr "\nQUIT\n"
      go s (c:cs)
        | changing  = putStr "\ESCc" >> putStrLn (draw nextState)
                    >> go nextState cs
        | otherwise = go s cs
        where
          changing  = elem c actionLetters
          nextState = (handle (KeyPress [c]) s)


main :: IO ()
main = runInteraction $ resettable . withStartScreen . withUndo
       $ Interaction initialState handleEvent draw
