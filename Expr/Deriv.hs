module Expr.Deriv (d) where

import Expr.Base

d :: (Eq a, Floating b) => a -> Expr a b -> Expr a b
d x (Var y) 
  | x == y = 1 
  | otherwise = 0
d x (Lit n) = 0
d x (Add e1 e2) = (d x e1) + (d x e2)
d x (Sub e1 e2) = (d x e1) - (d x e2)
d x (Mul e1 e2) = ((d x e1) * e2) + (e1 * (d x e2))
d x (Div e1 e2) = ((d x e1) * e2) - (e1 * (d x e2)) / (e2 * e2)
d x (Pow e1 e2) = (e1 ** e2) * ((d x e1) * e2 / e1 + (d x e2) * log e1)
d x (Sum es) = Sum $ map (d x) es
d x (Prod (e:es)) = (Prod $ (d x e):es) + (e * (d x (Prod es)))
d x (Func f e) = (df f e) * (d x e)

df :: (Floating b) => Function -> Expr a b -> Expr a b
df Ln = recip
df Sin = cos
df Cos = negate . sin
df Tan =  \e -> (1 / (cos e)) ^ 2
df ASin = \e -> 1 / sqrt (1 - e**2)
df ACos = \e -> -1 / sqrt (1 - e**2)
df ATan = \e -> 1 / (1 + e**2)
df Sinh = cosh
df Cosh = sinh
df Tanh =  \e -> 1 - (tanh e)^2
df ASinh = \e -> 1 / sqrt (e^2 + 1)
df ACosh = \e -> 1 / sqrt (e^2 - 1)
df ATanh = \e -> 1 / (1 - e**2)

