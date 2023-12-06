{-# LANGUAGE NamedFieldPuns #-}
module Part2.Tasks where

import Util(notImplementedYet)

data BinaryOp = Plus | Minus | Times deriving (Show, Eq)

data Term = IntConstant { intValue :: Int }          -- числовая константа
          | Variable    { varName :: String }        -- переменная
          | BinaryTerm  { op :: BinaryOp, lhv :: Term, rhv :: Term } -- бинарная операция
             deriving(Show,Eq)

infixl 6 |+| 
(|+|) :: Term -> Term -> Term
(|+|) l r = BinaryTerm{op = Plus, lhv = l, rhv = r}
infixl 6 |-| 
(|-|) :: Term -> Term -> Term
(|-|) l r = BinaryTerm{op = Minus, lhv = l, rhv = r}
infixl 7 |*| 
(|*|) :: Term -> Term -> Term
(|*|) l r = BinaryTerm{op = Times, lhv = l, rhv = r}

-- Заменить переменную `varName` на `replacement`
-- во всём выражении `expression`
replaceVar :: String -> Term -> Term -> Term
replaceVar varName replacement expression = case expression of 
   constant@IntConstant{intValue} -> constant
   variable@Variable{varName=varName'} -> if varName == varName' then replacement else variable
   BinaryTerm{op, lhv=lhv', rhv=rhv'} -> BinaryTerm
      {
         op = op, 
         lhv = replaceVar varName replacement lhv', 
         rhv = replaceVar varName replacement rhv'
      }

-- Посчитать значение выражения `Term`
-- если оно состоит только из констант
evaluate :: Term -> Term
evaluate t = helper t
   where
      helper :: Term -> Term
      helper t = case t of
         constant@IntConstant{intValue} -> constant
         var@Variable{} -> var
         BinaryTerm{op=Plus, lhv=IntConstant{intValue=a}, rhv=IntConstant{intValue=b}} -> IntConstant{intValue=a + b}
         BinaryTerm{op=Minus, lhv=IntConstant{intValue=a}, rhv=IntConstant{intValue=b}} -> IntConstant{intValue=a - b}
         BinaryTerm{op=Times, lhv=IntConstant{intValue=a}, rhv=IntConstant{intValue=b}} -> IntConstant{intValue=a * b}
         bin@BinaryTerm{op, lhv=Variable{}, rhv=Variable{}} -> bin
         BinaryTerm{op, lhv=var@Variable{}, rhv} -> BinaryTerm{op, lhv=var, rhv=helper rhv}
         BinaryTerm{op, lhv, rhv=var@Variable{}} -> BinaryTerm{op, lhv= helper lhv, rhv=var}
         -- BinaryTerm{op, lhv, rhv} -> helper BinaryTerm{op=op, lhv=helper lhv, rhv=helper rhv}
         BinaryTerm{op, lhv, rhv} -> case (helper lhv, helper rhv) of
            (x@IntConstant{}, y@IntConstant{}) -> helper $ BinaryTerm op x y
            (x, y) -> BinaryTerm op x y
