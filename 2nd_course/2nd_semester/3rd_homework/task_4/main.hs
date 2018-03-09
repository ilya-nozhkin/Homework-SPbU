brackets = [('(', ')'), ('[', ']'), ('{', '}')]
corresponds a b = any (\p -> (fst p) == a && (snd p) == b) brackets
isOpening a = elem a (map fst brackets)
isClosing a = elem a (map snd brackets)

checkBrackets s = checkBrackets' [] s where
    checkBrackets' [] "" = True
    checkBrackets' _ "" = False
    checkBrackets' a (c:s) 
      | isOpening c = checkBrackets' (c:a) s
      | isClosing c = if not (null a) && corresponds (head a) c then checkBrackets' (tail a) s else False
      | otherwise = checkBrackets' a s

main :: IO ()
main = do
    putStrLn "Enter a text with brackets: "
    line <- getLine

    let output = checkBrackets line

    putStrLn ("Bracket sequence in this text is " ++ if output then "correct" else "incorrect")
