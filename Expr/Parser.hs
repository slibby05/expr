module Expr.Parser (expr) where

import Expr.Base
import Text.Parsec
import Text.Parsec.String
import Text.ParserCombinators.Parsec.Number

{-
grammar for expression
 E -> E + T | E - T | T
 T -> T * P | T / P | P
 P -> P ^ Fn | Fn
 Fn -> fun F | F
 F -> ( E ) | var | id
 fun -> ln | exp | sin | cos | tan
 var = L(L|D)*
 num = DD*\.D*eD*
-}
--Parsec version
expr :: String -> Expr String Double
expr s = case (parse exprs "" s) of
            Left err -> undefined
            Right x  -> x

exprs :: Parser (Expr String Double)
exprs = term >>= expr'

expr' x = exprP <|> exprM <|> (return x)
 where
  exprP = char '+' >> term >>= expr' >>= return . (x+)
  exprM = char '-' >> term >>= expr' >>= return . (x-)

term :: Parser (Expr String Double)
term = power >>= term'

term' x = termM <|> termD <|> (return x)
 where
  termM = char '*' >> power >>= term' >>= return . (x*)
  termD = char '/' >> power >>= term' >>= return . (x/)

power :: Parser (Expr String Double)
power = function >>= power'

power' x = powerP <|> (return x)
 where
  powerP = char '^' >> function >>= power' >>= return . (x**)

function :: Parser (Expr String Double)
function = (string "ln("  >> functionF Ln)  <|>
           (string "exp(" >> functionF Exp) <|>
           (string "sin(" >> functionF Sin) <|>
           (string "cos(" >> functionF Cos) <|>
           (string "tan(" >> functionF Tan) <|>
           (string "asin(" >> functionF ASin) <|>
           (string "acos(" >> functionF ACos) <|>
           (string "atan(" >> functionF ATan) <|>
           (string "sinh(" >> functionF Sinh) <|>
           (string "cosh(" >> functionF Cosh) <|>
           (string "tanh(" >> functionF Tanh) <|>
           (string "asinh(" >> functionF ASinh) <|>
           (string "acosh(" >> functionF ACosh) <|>
           (string "atanh(" >> functionF ATanh) <|>
           factor
 where
  functionF f = exprs >>= (\e -> char ')' >> return (Func f e))

factor :: Parser (Expr String Double)
factor = (char '(' >> exprs >>= (\e -> char ')' >> return e)) <|>
         (floating >>= return . Lit)   <|>
         (many1 letter >>= return . Var)

