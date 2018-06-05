module Lexer where

import Data.Char

data TokenType = Number | Plus | Minus | Star | Slash | OpnBracket | ClsBracket | Variable | Intermediate | Unknown
tokenTypeSymbols = [(Plus, '+'), (Minus, '-'), (Star, '*'), (Slash, '/'), (Variable, 'x'), (OpnBracket, '('), (ClsBracket, ')')]

tsum = Token Plus "+"
tdiff = Token Minus "-"
tmult = Token Star "*"
tquot = Token Slash "/"

instance Eq TokenType where
    Number == Number = True
    Plus == Plus = True
    Minus == Minus = True
    Star == Star = True
    Slash == Slash = True
    Variable == Variable = True
    OpnBracket == OpnBracket = True
    ClsBracket == ClsBracket = True
    _ == _ = False

data Token = Token TokenType String
ttype (Token t _) = t
tvalue (Token _ v) = v

instance Eq Token where
    a == b = ((ttype a) == (ttype b)) && ((tvalue a) == (tvalue b))

instance Show Token where
    show (Token t v) = show v

matchNumber = (\m -> if (null m) then Nothing else Just (Token Number m)) . (takeWhile isDigit)
matchSymbol (t, c) (s:_) = if (s == c) then Just (Token t [c]) else Nothing

matchers = [matchNumber] ++ (map (matchSymbol) tokenTypeSymbols)
match s = ((maybe (Token Unknown "") (\t -> t)) . 
           (foldr (\r f -> maybe f (\_ -> r) r) Nothing) .
           (map (\m -> m s))) matchers

lexer' "" = []
lexer' s = lexer'' $ match s where
    lexer'' t = t:(lexer (drop (length (tvalue t)) s))

lexer = lexer' . (filter (\t -> t /= ' '))
