module Part5.Tasks where

import Util(notImplementedYet)

-- Реализуйте левую свёртку
myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl _ acc [] = acc
myFoldl f acc (head : tail) = myFoldl f (f acc head) tail

-- Реализуйте правую свёртку
myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _ acc [] = acc
myFoldr f acc (head : tail) = f head (myFoldr f acc tail)

-- Используя реализации свёрток выше, реализуйте все остальные функции в данном файле

myMap :: (a -> b) -> [a] -> [b]
myMap f = myFoldr (\x acc -> f x : acc) []

myConcatMap :: (a -> [b]) -> [a] -> [b]
myConcatMap f = myConcat . myMap f

myConcat :: [[a]] -> [a]
myConcat = myFoldr (++) []

myReverse :: [a] -> [a]
myReverse = myFoldl (flip (:)) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p = myFoldr (\x acc -> if p x then x : acc else acc) []

myPartition :: (a -> Bool) -> [a] -> ([a], [a])
myPartition p = myFoldr (\x (matches, nonMatches) ->
    if p x then (x : matches, nonMatches) else (matches, x : nonMatches)
  ) ([], [])

