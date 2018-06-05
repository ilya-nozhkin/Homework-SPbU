module Simplification where

import Data.Tree
import Lexer

simplify (Node (Token Number s) []) = Node (Token Number s) []
simplify (Node (Token Variable s) []) = Node (Token Variable s) []

simplify (Node t [l, r]) = simplify'' t l r where
    simplify'' t l r = simplify' t (simplify l) (simplify r)

    -- t +|- 0 -> t
    simplify' (Token Plus _) (Node (Token Number "0") []) t = t

    simplify' (Token Minus _) t (Node (Token Number "0") []) = t

    -- 1 * t -> t; 0 * t -> 0
    simplify' (Token Star _) (Node (Token Number "1") []) t = t
    simplify' (Token Star _) (Node (Token Number "0") []) t = Node (Token Number "0") []

    -- t / 1 -> t; 0 / t -> 0 (assume that 0 / 0 is not possible)
    simplify' (Token Slash _) t (Node (Token Number "1") []) = t
    simplify' (Token Slash _) (Node (Token Number "0") []) t = Node (Token Number "0") []

    -- a + b -> "a + b"; a - b -> "a - b"
    simplify' (Token Plus _) (Node (Token Number a) []) (Node (Token Number b) []) = 
        Node (Token Number (show ((read a :: Integer) + (read b :: Integer)))) []
    simplify' (Token Minus _) (Node (Token Number a) []) (Node (Token Number b) []) = 
        Node (Token Number (show ((read a :: Integer) - (read b :: Integer)))) []

    -- a * b -> "a * b"
    simplify' (Token Star _) (Node (Token Number a) []) (Node (Token Number b) []) = 
        Node (Token Number (show ((read a :: Integer) * (read b :: Integer)))) []

    -- x * a -> a * x; x + a -> a + x
    simplify' (Token Star _) x (Node (Token Number s) []) = simplify'' tmult (Node (Token Number s) []) x
    simplify' (Token Plus _) x (Node (Token Number s) []) = simplify'' tsum  (Node (Token Number s) []) x

    -- t1 - (t2 + t3) = (t1 - t2) - t3
    simplify' (Token Minus _) t1 (Node (Token Plus _) [t2, t3]) = 
        simplify'' tdiff (Node tdiff [t1, t2]) t3

    -- t1 - (t2 - t3) = (t1 - t2) + t3
    simplify' (Token Minus _) t1 (Node (Token Minus _) [t2, t3]) = 
        simplify'' tsum (Node tdiff [t1, t2]) t3

    -- t1 * (t2 + t3) = t1 * t2 + t1 * t3
    simplify' (Token Star _) t1 (Node (Token Plus _) [t2, t3]) = 
        simplify'' tsum (Node tmult [t1, t2]) (Node tmult [t1, t3])

    -- (t2 + t3) * t1 = t1 * t2 + t1 * t3
    simplify' (Token Star _) (Node (Token Plus _) [t2, t3]) t1 = 
        simplify'' tsum (Node tmult [t1, t2]) (Node tmult [t1, t3])

    -- t1 * (t2 - t3) = t1 * t2 - t1 * t3
    simplify' (Token Star _) t1 (Node (Token Minus _) [t2, t3]) = 
        simplify'' tdiff (Node tmult [t1, t2]) (Node tmult [t1, t3])

    -- (t2 - t3) * t1 = t1 * t2 - t1 * t3
    simplify' (Token Star _) (Node (Token Minus _) [t2, t3]) t1 = 
        simplify'' tdiff (Node tmult [t1, t2]) (Node tmult [t1, t3])

    -- -a * t -> 0 - a*t
    simplify' (Token Star _) (Node (Token Number a) []) t
      | (read a :: Integer) < 0 = simplify'' tdiff (Node (Token Number "0") []) (Node tmult [Node (Token Number (tail a)) [], t])
      | otherwise = Node tmult [Node (Token Number a) [], t]

    -- -a / t -> 0 - a/t
    simplify' (Token Slash _) (Node (Token Number a) []) t
      | (read a :: Integer) < 0 = simplify'' tdiff (Node (Token Number "0") []) (Node tquot [Node (Token Number (tail a)) [], t])
      | otherwise = Node tquot [Node (Token Number a) [], t]

    simplify' t l r = Node t [l, r]
