module Part2.Tasks where

import Util(notImplementedYet)

data BinaryOp = Plus | Minus | Times deriving (Show, Eq)

data Term = IntConstant { intValue :: Int }          -- числовая константа
          | Variable    { varName :: String }        -- переменная
          | BinaryTerm  { op :: BinaryOp, lhv :: Term, rhv :: Term } -- бинарная операция
             deriving(Show,Eq)

-- Для бинарных операций необходима не только реализация, но и адекватные
-- ассоциативность и приоритет
(|+|) :: Term -> Term -> Term
(|+|) = BinaryTerm Plus
infixl 6 |+|
(|-|) :: Term -> Term -> Term
(|-|) = BinaryTerm Minus
infixl 6 |-|
(|*|) :: Term -> Term -> Term
(|*|) = BinaryTerm Times
infixl 7 |*|

-- Заменить переменную `varName` на `replacement`
-- во всём выражении `expression`
replaceVar :: String -> Term -> Term -> Term
replaceVar varName replacement expression = case expression of
   IntConstant _         -> expression
   BinaryTerm op lhv rhv -> BinaryTerm op (replaceVar varName replacement lhv) (replaceVar varName replacement rhv)
   Variable name
     | name == varName   -> replacement
     | otherwise         -> expression

-- Посчитать значение выражения `Term`
-- если оно состоит только из констант
evaluate :: Term -> Term
evaluate (BinaryTerm op lhv rhv) =
  case (op, evaluate lhv, evaluate rhv) of
    (Plus, IntConstant lhv, IntConstant rhv)  -> IntConstant (lhv + rhv)
    (Minus, IntConstant lhv, IntConstant rhv) -> IntConstant (lhv - rhv)
    (Times, IntConstant lhv, IntConstant rhv) -> IntConstant (lhv * rhv)
    (_, lhv, rhv)                             -> BinaryTerm op lhv rhv
evaluate term = term
