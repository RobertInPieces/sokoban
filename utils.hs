import Data.List

elemList :: Eq a => a -> [a] -> Bool
elemList _ []     = False
elemList a (x:xs) = a == x || elemList a xs

appendList :: [a] -> [a] -> [a]
appendList a b = a ++ b

listLength :: [a] -> Integer
listLength []     = 0
listLength (x:xs) = 1 + listLength xs

filterList :: (a -> Bool) -> [a] -> [a]
filterList _ [] = []
filterList cond (x:xs)
  | cond x    = [x] ++ filterList cond xs
  | otherwise = filterList cond xs

nth :: [a] -> Integer -> a
nth (x:xs) 1 = x
nth (x:xs) n = nth xs (n-1)

mapList :: (a -> b) -> [a] -> [b]
mapList _ []        = []
mapList func (x:xs) = [func x] ++ mapList func xs

andList :: [Bool] -> Bool
andList []     = True
andList (x:xs) = x && andList xs

allList :: (a-> Bool) -> [a] -> Bool
allList _ []        = True
allList cond (x:xs) = cond x && allList cond xs

foldList :: (a -> b -> b) -> b -> [a] -> b
foldList _ val []        = val
foldList func val (x:xs) = foldList func (func x val) xs

nodesToList :: Eq a => [a] -> [a] -> (a -> [a]) -> [a]
nodesToList old [] _         = old
nodesToList old curr getNext = nodesToList oldCurr new getNext where
  oldCurr    = old ++ curr
  removeOld  = filterList (\x -> not (elem x oldCurr))
  simplify   = nub . concat
  new        = removeOld (simplify (mapList getNext curr))

allGraph :: Eq a => (a -> Bool) -> a -> (a -> [a]) -> Bool
allGraph cond initial neighbours =
  allList cond (nodesToList [] [initial] neighbours)

isGraphClosed :: Eq a => a -> (a -> [a]) -> (a -> Bool) -> Bool
isGraphClosed initial neighbours isOk =
  allGraph isOk initial neighbours

reachable :: Eq a => a -> a -> (a -> [a]) -> Bool
reachable v initial neighbours =
  not (allGraph (/=v) initial neighbours)

allReachable :: Eq a => [a] -> a -> (a -> [a]) -> Bool
allReachable vs initial neighbours =
  allList (\x -> reachable x initial neighbours) vs
