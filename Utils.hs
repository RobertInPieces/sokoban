module Utils where

import Data.List

elemList :: Eq a => a -> [a] -> Bool
elemList _ []     = False
elemList a (x:xs) = a == x || elemList a xs

appendList :: [a] -> [a] -> [a]
appendList a [] = a
appendList [] b = b
appendList (x:xs) ys = x:appendList xs ys

listLength :: [a] -> Integer
listLength []     = 0
listLength (_:xs) = 1 + listLength xs

filterList :: (a -> Bool) -> [a] -> [a]
filterList _ [] = []
filterList cond (x:xs)
  | cond x    = x:rest
  | otherwise = rest
  where
    rest = filterList cond xs

nth :: [a] -> Integer -> a
nth (x:_)  1 = x
nth (_:xs) n = nth xs (n-1)
nth _ _      = error "Index out of bonds"

nthOrLast :: [a] -> Integer -> a
nthOrLast [x]    _ = x
nthOrLast (x:_)  1 = x
nthOrLast (_:xs) n = nthOrLast xs (n-1)
nthOrLast _      _ = error "Empty list"

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

nodesToList :: Eq a => [a] -> [a] -> (a -> [a]) -> (a -> Bool) -> [a]
nodesToList old curr getNext stopCondition
  | any stopCondition old = old
  | curr == []            = old
  | otherwise             = nodesToList oldCurr new getNext stopCondition where
    oldCurr    = old ++ curr
    removeOld  = filterList (\x -> not (elem x oldCurr))
    simplify   = nub . concat
    new        = removeOld (simplify (mapList getNext curr))

allGraph :: Eq a => (a -> Bool) -> a -> (a -> [a]) -> Bool
allGraph cond initial neighbours =
  allList cond (nodesToList [] [initial] neighbours notCond) where
    notCond = (\x -> not (cond x))

filterGraph :: Eq a => (a -> Bool) -> a -> (a -> [a]) -> (a -> Bool) -> [a]
filterGraph cond initial neighbours stopCondition =
  filterList cond (nodesToList [] [initial] neighbours stopCondition)

isGraphClosed :: Eq a => a -> (a -> [a]) -> (a -> Bool) -> Bool
isGraphClosed initial neighbours isOk =
  allGraph isOk initial neighbours

reachable :: Eq a => a -> a -> (a -> [a]) -> Bool
reachable v initial neighbours =
  not (allGraph (/=v) initial neighbours)

allReachable :: Eq a => [a] -> a -> (a -> [a]) -> Bool
allReachable vs initial neighbours =
  allList (\x -> reachable x initial neighbours) vs
