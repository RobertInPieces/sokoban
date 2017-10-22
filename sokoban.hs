import CodeWorld

main :: Program
main = program
type Program = IO ()

program :: Program
program = drawingOf (pictureOfMaze)

wallCol, groundCol, storageCol, boxCol, emptyCol :: Color
wallCol    = RGBA 0.39 0.39 0.39 1
groundCol  = RGBA 0.89 0.8 0.45 1
storageCol = RGBA 1 0.39 0.39 1
boxCol     = RGBA 0.56 0.2 0 1
emptyCol   = white

wall, ground, storage, box, empty :: Picture
wall    = colored wallCol (solidRectangle 1 1)
ground  = colored groundCol (solidRectangle 1 1)
storage = colored storageCol (solidCircle (0.3)) & ground
box     = colored boxCol (solidRectangle 1 1)
empty   = colored emptyCol (solidRectangle 1 1)

drawTile :: Integer -> Picture
drawTile n =
  case n of
    1 -> wall
    2 -> ground
    3 -> storage
    4 -> box
    _ -> empty

maze :: Integer -> Integer -> Integer
maze x y
  | abs x > 4  || abs y > 4  = 0
  | abs x == 4 || abs y == 4 = 1
  | x ==  2 && y <= 0        = 1
  | x ==  3 && y <= 0        = 3
  | x >= -2 && y == 0        = 4
  | otherwise                = 2

drawTileFromMaze :: Integer -> Integer -> Picture
drawTileFromMaze x y = translated (fromIntegral x) (fromIntegral y) (drawTile (maze x y))

pictureOfMaze :: Picture
pictureOfMaze = pictures([drawTileFromMaze x y | x <- [-10..10], y <- [-10..10]])

