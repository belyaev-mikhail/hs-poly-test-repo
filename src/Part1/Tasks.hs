module Part1.Tasks where

import Util(notImplementedYet)

-- синус числа (формула Тейлора)
mySin :: Double -> Double
mySin x = sinN (normalizeAngle x)
sinN x = calculateSin x 0

normalizeAngle x = x - 2 * pi * fromIntegral (round (x / (2 * pi)))

calculateSin :: Double -> Integer -> Double
calculateSin x n
  | n == maxTerm = 0
  | otherwise =
    let termSign = if mod n 2 == 0 then 1.0 else -1.0
        termDegree = 2 * n + 1
    in calculateTerm x termSign termDegree + calculateSin x (n + 1)
    where maxTerm = 15

calculateTerm x termSign termDegree =
  termSign * (x ^ termDegree) / fromInteger (fact termDegree)

fact :: Integer -> Integer
fact 0 = 1
fact n = n * fact (n - 1)

-- косинус числа (формула Тейлора)
myCos :: Double -> Double
myCos x = mySin (x + pi / 2)

-- наибольший общий делитель двух чисел
myGCD :: Integer -> Integer -> Integer
myGCD 0 b = abs b
myGCD a b = myGCD (mod b a) a

-- является ли дата корректной с учётом количества дней в месяце и
-- вискокосных годов?
isDateCorrect :: Integer -> Integer -> Integer -> Bool
isDateCorrect dd mm year = dd > 0 && mm > 0 && year > 0 && case mm of
  1   -> dd < 32
  2   -> if (isYearLeap year) then dd < 30 else dd < 29
  3   -> dd < 32
  4   -> dd < 31
  5   -> dd < 32
  6   -> dd < 31
  7   -> dd < 32
  8   -> dd < 32
  9   -> dd < 31
  10  -> dd < 32
  11  -> dd < 31
  12  -> dd < 32
  invalidMonth -> False

isYearLeap year = (mod year 100 /= 0 && mod year 4 == 0) || (mod year 400 == 0)

-- возведение числа в степень, duh
-- готовые функции и плавающую арифметику использовать нельзя
myPow :: Integer -> Integer -> Integer
myPow x 0 = 1
myPow x y = x * myPow x (y - 1)

-- является ли данное число простым?
isPrime :: Integer -> Bool
isPrime x = any (\l -> mod x l == 0) [2 .. floor (sqrt (fromIntegral x))] == False

type Point2D = (Double, Double)

-- рассчитайте площадь многоугольника по формуле Гаусса
-- многоугольник задан списком координат
shapeArea :: [Point2D] -> Double
shapeArea points =
  let size = length points
      indexes = [0 .. (size - 1)]
  in 0.5 * abs(
    sum (map (\i -> fst (points !! i) * snd (points !! (mod (i + 1) size))) indexes)
    - sum (map (\i -> fst (points !! (mod (i + 1) size)) * snd (points !! i)) indexes)
  )

-- треугольник задан длиной трёх своих сторон.
-- функция должна вернуть
--  0, если он тупоугольный
--  1, если он остроугольный
--  2, если он прямоугольный
--  -1, если это не треугольник
triangleKind :: Double -> Double -> Double -> Integer
triangleKind a b c = if isValidTriangle a b c then calculateAngleType a b c else -1

isValidTriangle a b c = a + b > c && a + c > b && b + c > a

calculateAngleType a b c =
  let squaredMax = (maxOfThree a b c) ^ 2
      squaredDiffSize = a ^ 2 + b ^ 2 + c ^ 2 - squaredMax
  in case () of
    _ | squaredDiffSize < squaredMax -> 0
      | squaredDiffSize > squaredMax -> 1
      | otherwise                    -> 2

maxOfThree a b c = max a (max b c)
