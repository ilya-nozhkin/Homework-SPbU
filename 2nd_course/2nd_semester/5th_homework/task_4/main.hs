import Lexer
import Parser
import Derivatives
import Simplification

import Data.Tree

printExpression :: Tree Token -> String
printExpression (Node (Token Number s) []) = s
printExpression (Node (Token Variable s) []) = s
printExpression (Node (Token Plus _) [l, r]) = (printExpression l) ++ "+" ++ (printExpression r)

printExpression (Node (Token Minus _) [Node (Token Number "0") [], r]) = "-" ++ 
    (if ((ttype . rootLabel) r == Plus) || ((ttype . rootLabel) r == Minus) then "(" ++ (printExpression r) ++ ")" else (printExpression r))

printExpression (Node (Token Minus _) [l, r]) =(printExpression l) ++ "-" ++ 
    (if ((ttype . rootLabel) r == Plus) || ((ttype . rootLabel) r == Minus) then "(" ++ (printExpression r) ++ ")" else (printExpression r))

printExpression (Node (Token Star _) [l, r]) = 
    (if ((ttype . rootLabel) l == Plus) || ((ttype . rootLabel) l == Minus) then "(" ++ (printExpression l) ++ ")" else (printExpression l)) ++ "*" ++
    (if ((ttype . rootLabel) r == Plus) || ((ttype . rootLabel) r == Minus) then "(" ++ (printExpression r) ++ ")" else (printExpression r))

printExpression (Node (Token Slash _) [l, r]) = 
    (if ((ttype . rootLabel) l /= Number) && ((ttype . rootLabel) l /= Variable) then "(" ++ (printExpression l) ++ ")" else (printExpression l)) ++ "/" ++
    (if ((ttype . rootLabel) r /= Number) && ((ttype . rootLabel) r /= Variable) then "(" ++ (printExpression r) ++ ")" else (printExpression r))

derivativeOfExpression :: String -> String
derivativeOfExpression = (maybe "Invalid expression" (printExpression . simplify . derivative . simplify)) . parseExpression

main = getLine >>= putStrLn . derivativeOfExpression

