module Part3.Tasks where

import Util (notImplementedYet)
import Data.List (sort, group, maximumBy, groupBy, sortOn)
import Data.Function(on)
import Data.Char(digitToInt)

-- Функция finc принимает на вход функцию f и число n и возвращает список чисел [f(n), f(n + 1), ...]
finc :: (Int -> a) -> Int -> [a]
finc f n = result
    where 
        arrayOfInc = [n..]
        result = map f arrayOfInc

-- Функция ff принимает на вход функцию f и элемент x и возвращает список [x, f(x), f(f(x)), f(f(f(x))) ...]
ff :: (a -> a) -> a -> [a]
ff func a = helper a
    where 
        helper a = a : helper (func a)

-- Дан список чисел. Вернуть самую часто встречающуюся *цифру* в этих числах (если таковых несколько -- вернуть любую)
mostFreq :: [Int] -> Int
mostFreq arr = digitToInt $ fst $ maximumBy (compare `on` snd) freqList
  where digits = concatMap show arr
        freqList = map (\xs -> (head xs, length xs)) $ group $ sort digits

-- Дан список lst. Вернуть список элементов из lst без повторений, порядок может быть произвольным.
uniq :: (Eq a) => [a] -> [a]
uniq = helper []
  where
    helper used [] = []
    helper used (x:xs) = if elem x used
      then helper used xs
      else x : helper (x : used) xs

-- Функция grokBy принимает на вход список Lst и функцию F и каждому возможному
-- значению результата применения F к элементам Lst ставит в соответствие список элементов Lst,
-- приводящих к этому результату. Результат следует представить в виде списка пар.
grokBy :: (Eq k) => (a -> k) -> [a] -> [(k, [a])]
grokBy f l = map (\xs -> (snd . head $ xs, map fst xs))
  . groupBy (\x y -> snd x == snd y)
  $ map (\xs -> (xs, f xs)) l
