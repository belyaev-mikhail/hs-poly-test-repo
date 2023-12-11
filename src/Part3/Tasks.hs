module Part3.Tasks where

import Util (notImplementedYet)
import Data.Function (on)
import Data.List (maximumBy, groupBy)

-- Функция finc принимает на вход функцию f и число n и возвращает список чисел [f(n), f(n + 1), ...]
finc :: (Int -> a) -> Int -> [a]
finc f n = f n : finc f (n + 1)

-- Функция ff принимает на вход функцию f и элемент x и возвращает список [x, f(x), f(f(x)), f(f(f(x))) ...]
ff :: (a -> a) -> a -> [a]
ff f x = iterate f x

-- Дан список чисел. Вернуть самую часто встречающуюся *цифру* в этих числах (если таковых несколько -- вернуть любую)
mostFreq :: [Int] -> Int
mostFreq nums = read [mostFreqDigit]
  where
    allDigits = concatMap show nums
    mostFreqDigit = fst $ maximumBy (on compare snd) (timesEach "0123456789" allDigits)

timesEach digits allDigits = map (countDigit allDigits) digits
countDigit allDigits d = (d, length $ filter (== d) allDigits)

-- Дан список lst. Вернуть список элементов из lst без повторений, порядок может быть произвольным.
uniq :: (Eq a) => [a] -> [a]
uniq = foldr (\x acc -> if elem x acc then acc else x : acc) []

-- Функция grokBy принимает на вход список Lst и функцию F и каждому возможному
-- значению результата применения F к элементам Lst ставит в соответствие список элементов Lst,
-- приводящих к этому результату. Результат следует представить в виде списка пар.
grokBy :: (Eq k) => (a -> k) -> [a] -> [(k, [a])]
grokBy f l = map extractGroups (groupBy sameFValue $ map (\e -> (f e, e)) l)

sameFValue e1 e2 = fst e1 == fst e2
extractGroups lst = (fst (head lst), map snd lst)
