module Expr.Printer (show, showTree, prec, left, right) where

import Expr.Base

showTree :: (Show a, Show b) => Expr a b -> String
showTree (Var v) = show v
showTree (Lit n) = show n
showTree (Add e1 e2) = "(" ++ (showTree e1) ++ ") + (" ++ (showTree e2) ++ ")"
showTree (Sub e1 e2) = "(" ++ (showTree e1) ++ ") - (" ++ (showTree e2) ++ ")"
showTree (Mul e1 e2) = "(" ++ (showTree e1) ++ ") * (" ++ (showTree e2) ++ ")"
showTree (Div e1 e2) = "(" ++ (showTree e1) ++ ") / (" ++ (showTree e2) ++ ")"
showTree (Pow e1 e2) = "(" ++ (showTree e1) ++ ") ^ (" ++ (showTree e2) ++ ")"
showTree (Func f e) = (show f) ++ "(" ++ (show e) ++ ")"
showTree (Sum es) = "S " ++ (show es)
showTree (Prod es) = "P " ++ (show es)

instance (Show a, Show b) =>  Show (Expr a b) where
    show (Var v) = show v
    show (Lit n) = show n
    show (Func f e) = (show f) ++ "(" ++ (show e) ++ ")"
    show (Sum es) = "S " ++ (show es)
    show (Prod es) = "P " ++ (show es)
    --show e = show1 ++ (showOp e) ++ show2
    show e = showP ((show e1) ++ (showOp e) ++ (show e2))
     where
      showOp (Add _ _) = "+"
      showOp (Sub _ _) = "-"
      showOp (Mul _ _) = "*"
      showOp (Div _ _) = "/"
      showOp (Pow _ _) = "^"
      e1 = left e
      e2 = right e
      showP x
       | prec e < prec e1 && prec e < prec e2 = "(" ++ x ++ ")"
       | otherwise = x
      --show1
      -- | prec e <= prec e1 = "(" ++ (show e1) ++ ")"
      -- | otherwise  = show e1
      --show2
      -- | prec e <= prec e2 = "(" ++ (show e2) ++ ")"
      -- | otherwise  = show e2


prec :: Expr a b -> Int
prec (Add _ _) = 1
prec (Sub _ _) = 1
prec (Mul _ _) = 2
prec (Div _ _) = 2
prec (Pow _ _) = 3
prec (Func _ _) = 0
prec (Lit _) = 0
prec (Var _) = 0

left :: Expr a b -> Expr a b
left (Add e1 e2) = e1
left (Sub e1 e2) = e1
left (Mul e1 e2) = e1
left (Div e1 e2) = e1
left (Pow e1 e2) = e1
left e = undefined

right :: Expr a b -> Expr a b
right (Add e1 e2) = e2
right (Sub e1 e2) = e2
right (Mul e1 e2) = e2
right (Div e1 e2) = e2
right (Pow e1 e2) = e2
right e = undefined


instance Show Function where
  show Exp = "e^"
  show Ln = "ln"
  show Sin = "sin"
  show Cos = "cos"
  show Tan = "tan"
  show ASin = "sin^-1"
  show ACos = "cos^-1"
  show ATan = "tan^-1"
  show Sinh = "sinh"
  show Cosh = "cosh"
  show Tanh = "tanh"
  show ASinh = "sinh^-1"
  show ACosh = "cosh^-1"
  show ATanh = "tanh^-1"

