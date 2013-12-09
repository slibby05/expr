module Expr.Reduce (fix, reduce, apply) where

import Expr.Base

fix :: (Eq a) => (a -> a) -> a -> a
fix f x
 | x == (f x) = x
 | otherwise = fix f (f x)

reduce :: (Eq a) => Expr a Double -> Expr a Double
reduce = fix (rid . rzero . rlit)

{-
r (Add e1 e2) = (r e1) + (r e2)
r (Sub e1 e2) = (r e1) - (r e2)
r (Mul e1 e2) = (r e1) * (r e2)
r (Div e1 e2) = (r e1) / (r e2)
r (Pow e1 e2) = (r e1) ** (r e2)
r (Sum es) = Sum (map r es)
r (Prod es) = Prod (map r es)
r (Func f e) = Func f (r e)
r e = e
-}

rzero :: (Eq a) => Expr a Double -> Expr a Double
rzero (Mul (Lit 0) e) = Lit 0
rzero (Mul e (Lit 0)) = Lit 0
rzero (Pow e (Lit 0)) = Lit 1
rzero (Pow (Lit 0) e) = Lit 0
rzero (Pow (Lit 1) e) = Lit 1

rzero (Add e1 e2) = (rzero e1) + (rzero e2)
rzero (Sub e1 e2) = (rzero e1) - (rzero e2)
rzero (Mul e1 e2) = (rzero e1) * (rzero e2)
rzero (Div e1 e2) = (rzero e1) / (rzero e2)
rzero (Pow e1 e2) = (rzero e1) ** (rzero e2)
rzero (Sum es) = Sum (map rzero es)
rzero (Prod es) = Prod (map rzero es)
rzero (Func f e) = Func f (rzero e)
rzero e = e

rid :: (Eq a) => Expr a Double -> Expr a Double
rid (Add (Lit 0) e) = e
rid (Add e (Lit 0)) = e
rid (Sub e (Lit 0)) = e
rid (Mul (Lit 1) e) = e
rid (Mul e (Lit 1)) = e
rid (Div e (Lit 1)) = e
rid (Pow e (Lit 1)) = (rid e)
rid (Func Ln (Func Exp e)) = e
rid (Func Exp (Func Ln e)) = e

rid (Add e1 e2) = (rid e1) + (rid e2)
rid (Sub e1 e2) = (rid e1) - (rid e2)
rid (Mul e1 e2) = (rid e1) * (rid e2)
rid (Div e1 e2) = (rid e1) / (rid e2)
rid (Pow e1 e2) = (rid e1) ** (rid e2)
rid (Func f e) = Func f (rid e)
rid (Prod es) = Prod (filter (/=(Lit 0)) $ map rid $ es)
rid (Sum es)  = Sum  (filter (/=(Lit 1)) $ map rid $ es)
rid e = e

rlit :: (Eq a) => Expr a Double -> Expr a Double
rlit (Add (Lit l1) (Lit l2)) = Lit $ l1 + l2
rlit (Sub (Lit l1) (Lit l2)) = Lit $ l1 - l2
rlit (Mul (Lit l1) (Lit l2)) = Lit $ l1 * l2
rlit (Div (Lit l1) (Lit l2)) = Lit $ l1 / l2
rlit (Pow (Lit l1) (Lit l2)) = Lit $ l1 ** l2
rlit (Func f (Lit c)) = Lit $ apply f c
rlit (Sum []) = Lit 0
rlit (Prod []) = Lit 0
rlit (Sum es)
 | null lits = Sum nonLits
 | otherwise  = Sum ((foldr litAdd (Lit 0) lits) : nonLits)
  where
   lits = filter isLit (map rlit es)
   nonLits = filter (not . isLit) es
   litAdd (Lit a) (Lit b) = Lit (a+b)
   litAss e1 e2 = e1 + e2
rlit (Prod es)
 | null lits = Prod nonLits
 | otherwise  = Prod ((foldr (|*|) (Lit 1) lits) : nonLits)
  where
   lits = filter isLit (map rlit es)
   nonLits = filter (not . isLit) es
   litProd (Lit a) (Lit b) = Lit (a*b)
   litProd e1 e2 = e1 * e2

rlit (Add e1 e2) = (rlit e1) + (rlit e2)
rlit (Sub e1 e2) = (rlit e1) - (rlit e2)
rlit (Mul e1 e2) = (rlit e1) * (rlit e2)
rlit (Div e1 e2) = (rlit e1) / (rlit e2)
rlit (Pow e1 e2) = (rlit e1) ** (rlit e2)
rlit (Func f e) = Func f (rlit e)
rlit e = e

apply :: Function -> Double -> Double
apply Exp = exp
apply Ln = log
apply Sin = sin
apply Cos = cos
apply Tan = tan

