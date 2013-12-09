module Expr.Base (Expr(..), Function(..), isLit, isVar) where

import Data.Ratio

data Expr a b = Var a
              | Lit b
              | Add (Expr a b) (Expr a b)
              | Sub (Expr a b) (Expr a b)
              | Mul (Expr a b) (Expr a b)
              | Div (Expr a b) (Expr a b)
              | Pow (Expr a b) (Expr a b)
              | Sum [Expr a b]
              | Prod [Expr a b]
              | Func Function (Expr a b)
              deriving(Eq, Ord)

data Function = Exp | Ln | Sin | Cos | Tan | ASin | ACos | ATan
              | Sinh | Cosh | Tanh | ASinh | ACosh | ATanh
              deriving(Eq, Ord)


isLit :: Expr a b -> Bool
isLit (Lit _) = True
isLit _ = False

isVar :: Expr a b -> Bool
isVar (Var _) = True
isVar _ = False


{-
Proof that fmap satisfys functor laws
  
fmap id Lit l    = Lit (id l) = Lit l

fmap id Var v    = Var v

fmap id Op e1 e2 = Op (fmap id e1) (fmap id e2) 
                 = Op e1 e2

fmap id Ops es   = Ops map (fmap id) es 
                 = Ops [(fmap id) e | e <- es]
                 = Ops [e | e <- es]
                 = Ops es

fmap id Func F e = Func F (fmap id e) 
                 = Func F e

fmap (f . g) Lit l = Lit ((f . g) l)
                   = Lit (f (g l))
                   = fmap f . Lit (g l)
                   = (fmap f . fmap g) Lit l

fmap (f . g) Var v      = Var v
(fmap f . fmap g) Var v = fmap f (fmap g Var v) 
                        = fmap f Var v 
                        = Var v

fmap (f . g) Op e1 e2 = Op (fmap (f . g) e1) (fmap (f . g) e2)
                      = Op (fmap f . fmap g) e1 (fmap f . fmap g) e2
                      = fmap f $ Op (fmap g) e1 (fmap g) e2
                      = (fmap f . fmap g) Op e1 e2

fmap (f . g) Ops es = Ops map (fmap (f . g)) es
                    = Ops [fmap (f . g) e | e <- es]
                    = Ops [(fmap f . fmap g) e | e <- es]
                    = Ops map (fmap f . fmap g) es
                    = fmap f $ Ops map (fmap g) es
                    = (fmap f . fmap g) Ops es

fmap (f . g) Func F e = Func F (fmap (f . g) e)
                      = Func F ((fmap f . fmap g) e)
                      = Func F (fmap f (fmap g e))
                      = fmap f . Func F (g e)
                      = (fmap f . fmap g) Func F e
-}
instance Functor (Expr a) where
    fmap f (Lit l)     = Lit $ f l
    fmap f (Var v)     = Var v
    fmap f (Add e1 e2) = Add (fmap f e1) (fmap f e2)
    fmap f (Sub e1 e2) = Sub (fmap f e1) (fmap f e2)
    fmap f (Mul e1 e2) = Mul (fmap f e1) (fmap f e2)
    fmap f (Div e1 e2) = Div (fmap f e1) (fmap f e2)
    fmap f (Pow e1 e2) = Pow (fmap f e1) (fmap f e2)
    fmap f (Sum es)    = Sum  (map (fmap f) es)
    fmap f (Prod es)   = Prod (map (fmap f) es)
    fmap f (Func ef e) = Func ef (fmap f e)

neg :: Num b => Expr a b -> Expr a b
neg (Lit l) = Lit (negate l)
neg (Var v) = (Lit (-1)) * (Var v)
neg (Add a b) = (neg a) - b
neg (Sub a b) = (neg a) + b
neg (Mul a b) = (neg a) * b
neg (Div a b) = Div (neg a) b
neg (Pow a b) = (Lit (-1)) * (Pow a b)
neg (Sum es) = Sum (map neg es)
neg (Prod []) = Prod []
neg (Prod (e:es)) = Prod ((neg e):es)
neg (Func f e) = (Lit (-1)) * (Func f e)

instance (Num b) => Num (Expr a b) where
    e1 + e2 = Add e1 e2
    e1 - e2 = Sub e1 e2
    e1 * e2 = Mul e1 e2
    negate = neg
    abs e = e
    signum e = e
    fromInteger i = Lit $ fromInteger i

instance (Fractional b) => Fractional (Expr a b) where
    e1 / e2 = Div e1 e2
    recip e = 1 / e
    fromRational a = (Lit $ fromInteger $ numerator a) / (Lit $ fromInteger $ denominator a)

instance (Floating b) => Floating (Expr a b)  where
    pi = Lit pi
    exp e = Func Exp e
    log e = Func Ln e
    sqrt e = Pow e (Lit (1 / 2))
    e1 ** e2 = Pow e1 e2
    logBase e (Lit b) = (Func Ln e) / (Func Ln (Lit (1 / b)))
    logBase e _ = undefined
    sin a = Func Sin a
    cos a = Func Cos a
    tan a = Func Tan a
    asin a = Func ASin a
    acos a = Func ACos a
    atan a = Func ATan a
    sinh a = Func Sinh a
    cosh a = Func Cosh a
    tanh a = Func Tanh a
    asinh a = Func ASinh a
    acosh a = Func ACosh a
    atanh a = Func ATanh a
