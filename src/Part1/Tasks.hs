module Part1.Tasks where

import Util(notImplementedYet)

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

simply :: Double -> Double
simply x = x - 2 * pi * (fromIntegral . round $ (x / (2 * pi)))

-- синус числа (формула Тейлора)
mySin :: Double -> Double
mySin x = sum (simply x) 0 where
    helper :: Double -> Integer -> Double
    helper x k = ((-1) ** fromIntegral k) * ((x ** (2 * fromIntegral k + 1)) / fromIntegral (factorial (2 * k + 1)))

    sum :: Double -> Integer -> Double
    sum x 100 = 0
    sum x k = helper x k + sum x (k + 1)

-- косинус числа (формула Тейлора)
myCos :: Double -> Double
myCos x = sum (simply x) 0 where
    helper :: Double -> Integer -> Double
    helper x k = ((-1) ** fromIntegral k) / fromIntegral (factorial (2 * fromIntegral k)) * x ** (2 * fromIntegral k)

    sum :: Double -> Integer -> Double
    sum x 100 = 0
    sum x k = helper x k + sum x (k + 1)

-- наибольший общий делитель двух чисел
myGCD :: Integer -> Integer -> Integer
myGCD a b = helper (abs a) (abs b) where
    getR :: Integer -> Integer -> Integer
    getR a b
        | b > a = getR b a 
        | b == 0 = a
        | otherwise = a `mod` b

    helper :: Integer -> Integer -> Integer
    helper a b 
        | b > a = helper b a
        | b == 0 = a
        | otherwise = if r == 0 then min a b else helper (min a b) r
            where
                r = getR a b 

        

-- является ли дата корректной с учётом количества дней в месяце и
-- вискокосных годов?
isDateCorrect :: Integer -> Integer -> Integer -> Bool
isDateCorrect day month year
    | elem month [1, 3, 5, 7, 8, 10, 12] = day <= 31
    | elem month [4, 6, 9, 11] = day <= 30
    | month == 2 = if isVisokosniy year then day <= 29 else day <= 28
        where
            isVisokosniy :: Integer -> Bool
            isVisokosniy year = (year `mod` 4 == 0 && year `mod` 100 /= 0) || year `mod` 400 == 0


-- возведение числа в степень, duh
-- готовые функции и плавающую арифметику использовать нельзя
myPow :: Integer -> Integer -> Integer
myPow x n
    | n == 0 = 1
    | n > 0 = x * myPow x (n - 1)

-- является ли данное число простым?
isPrime :: Integer -> Bool
isPrime n = helper n 2
    where
        helper :: Integer -> Integer -> Bool
        helper n a
            | n == 1 = False
            | a > round (sqrt (fromIntegral n)) = True
            | otherwise = if n `mod` a == 0 then False else helper n (a + 1)

type Point2D = (Double, Double)

-- рассчитайте площадь многоугольника по формуле Гаусса
-- многоугольник задан списком координат
shapeArea :: [Point2D] -> Double
--shapeArea points = notImplementedYet
shapeArea points = 0.5 * abs
    (sum [ x !! i * y !! (i + 1) - x !! (i + 1) * y !! i | i <- [0..n - 2] ] + last x * head y - last y * head x)
  where 
    (x, y) = unzip points
    n = length points

-- треугольник задан длиной трёх своих сторон.
-- функция должна вернуть
--  0, если он тупоугольный
--  1, если он остроугольный
--  2, если он прямоугольный
--  -1, если это не треугольник
triangleKind :: Double -> Double -> Double -> Integer
triangleKind a b c 
    | a <= 0 || b <= 0 || c <= 0 = -1
    | a + b <= c || a + c <= b || b + c <= a = -1
    | a ** 2 + b ** 2 == c ** 2 || a ** 2 + c ** 2 == b ** 2 || b ** 2 + c ** 2 == a ** 2 = 2
    | a ** 2 + b ** 2 < c ** 2 || a ** 2 + c ** 2 < b ** 2 || b ** 2 + c ** 2 < a ** 2 = 0
    | otherwise = 1
