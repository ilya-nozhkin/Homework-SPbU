module Derivatives where

import Data.Tree
import Lexer

derivative :: Tree Token -> Tree Token

derivative (Node (Token Plus _)  [l, r]) = Node tsum  [derivative l, derivative r]
derivative (Node (Token Minus _) [l, r]) = Node tdiff [derivative l, derivative r]
derivative (Node (Token Star _)  [l, r]) = Node tsum  [Node tmult [derivative l, r], Node tmult [l, derivative r]]
derivative (Node (Token Slash _) [l, r]) = Node tdiff [Node tquot [derivative l, r], Node tquot [Node tmult [l, derivative r], Node tmult [r, r]]]
derivative (Node (Token Number s) [])    = Node (Token Number "0") []
derivative (Node (Token Variable s) [])  = Node (Token Number "1") []
