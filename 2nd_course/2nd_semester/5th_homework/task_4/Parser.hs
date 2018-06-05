module Parser where

import Data.Tree
import Data.List
import Lexer

-- We'll implement our own parser combinators with left-recursive grammars and ambiguous derivations!
type Parser = ([Token] -> [(Tree Token, [Token])])

term :: TokenType -> Parser
term t = (\l -> if (null l) then [] else 
            (if t == (ttype (head l)) then [(Node (head l) [], tail l)] else []))

(~>) :: Parser -> Parser -> Parser
a ~> b = \l -> concatMap (\(t1, r) -> map (\(t2, f) -> (Node (Token Intermediate "") [t2, t1], f)) (a r)) (b l)
    
(<|>) :: Parser -> Parser -> Parser
a <|> b = \l -> concat [a l, b l]

parse :: Parser -> [Token] -> Maybe (Tree Token)
parse p = (find (\_ -> True)) . (map fst) . (filter (null . snd)) . p . reverse

-- Grammar
expression :: Parser

atom = (term Number) <|> 
       ((term Minus) ~> (term Number)) <|>
       (term Variable) <|> 
       ((term Minus) ~> (term Variable)) <|>
       ((term OpnBracket) ~> expression ~> (term ClsBracket)) <|>
       ((term Minus) ~> ((term OpnBracket) ~> expression ~> (term ClsBracket)))
prod_or_quot = (prod_or_quot ~> (term Star) ~> atom) <|> 
               (prod_or_quot ~> (term Slash) ~> atom) <|> 
                atom
plus_or_minus = (plus_or_minus ~> (term Plus) ~> prod_or_quot) <|>
                (plus_or_minus ~> (term Minus) ~> prod_or_quot) <|>
                 prod_or_quot
expression = plus_or_minus

prettify :: Tree Token -> Tree Token

-- remove brackets
prettify (Node (Token Intermediate _) [Node (Token Intermediate _) [Node (Token OpnBracket _) _, e], Node (Token ClsBracket _) _]) = 
    prettify e

-- remove intermediate nodes
prettify (Node (Token Intermediate _) [Node (Token Minus _) [], e]) = 
    Node (Token Star "*") [Node (Token Number "-1") [], prettify e]
prettify (Node (Token Intermediate _) [Node (Token Intermediate _) [v1, Node t2 []], v3]) = 
    Node t2 [prettify v1, prettify v3]

prettify n = n

parseExpression = (maybe Nothing (Just . prettify)) . (parse expression) . lexer
