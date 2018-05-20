import Data.List.Split
import Data.List

data Record = Record {name :: String, phone :: String}
data Phonebook = Phonebook {phones :: [Record]}

addRecord record (Phonebook phones) = Phonebook (record:phones)
findPhoneByName wanted (Phonebook phones) = (maybe Nothing (Just . phone) . find ((wanted ==) . name)) phones
findNameByPhone wanted (Phonebook phones) = (maybe Nothing (Just . name) . find ((wanted ==) . phone)) phones
saveIntoFile (Phonebook phones) = writeFile "phonebook.txt" $ concatMap (\r -> (name r) ++ " " ++ (phone r) ++ "\n") phones
readFromFile = readFile "phonebook.txt" >>= return . Phonebook . map ((\(n:p:_) -> Record n p) . words) . (splitOn "\n")

interpret :: Phonebook -> String -> IO Phonebook
interpret l ('0':' ':cmd) = return l
interpret l ('1':' ':cmd) = getLine >>= interpret (addRecord ((\(n:p:_) -> Record n p) (words cmd)) l)
interpret l ('2':' ':cmd) = (putStrLn . show) (findPhoneByName cmd l) >> getLine >>= interpret l
interpret l ('3':' ':cmd) = (putStrLn . show) (findNameByPhone cmd l) >> getLine >>= interpret l 
interpret l ('4':_) = saveIntoFile l >> getLine >>= interpret l
interpret _ ('5':_) = readFromFile >>= (\l -> getLine >>= interpret l)
interpret l _ = putStrLn "Unknown operation" >> getLine >>= interpret l

main :: IO ()
main = getLine >>= interpret (Phonebook []) >> return ()
