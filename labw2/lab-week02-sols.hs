{- 
Part 1: 

Note that the Boolean type is given as a standard datatype with just the two 
constants True and False (as indicated in lectures)

lines function uses several techniques: 
- a section (== '\n') is a function of type Char -> Bool that returns True if its 
argument is '\n\
- a case expression
- recursion
The algorithm is as follows: first call break to split the input into the first line 
and the rest. Then use recursion to split the rest into lines. Finally cons the first 
line onto the remaining lines to get the complete list of lines. 

unlines: This again uses a section (++ "\n") inserts a newline after a string. 
concatMap applies a function to each element of a list and then concatenates the results. 
So concatMap (++ "\n") adds a newline to each string and then concatenates. 

-}

-- Boolean type  
 
-- data  Bool  =  False | True     deriving (Eq, Ord, Enum, Read, Show, Bounded) 


-- lines breaks a string up into a list of strings at newline characters.  
-- The resulting strings do not contain newlines.  Similary, words  
-- breaks a string up into a list of words, which were delimited by  
-- white space.  unlines and unwords are the inverse operations.  
-- unlines joins lines with terminating newlines, and unwords joins  
-- words with separating spaces.  
 
lines'            :: String -> [String]  
lines' ""         =  []  
lines' s          =  let (l, s') = break (== '\n') s  
                      in  l : case s' of  
                                []      -> []  
                                (_:s'') -> lines' s'' 
                                
unlines'          :: [String] -> String  
unlines'          =  concatMap (++ "\n")                                 

concatMap' :: (a -> [b]) -> [a] -> [b]  
concatMap' f = concat . map f 


{-
PART 2
-}

-- processFile takes an operation and two filenames. 
-- It reads in the first file as a String, and then applies the operation to that String
-- Finally it saves the resulting string into the second file
-- usage : processFile (map toUpper) "input.txt" "output.txt"
processFile :: (String -> String) -> FilePath -> FilePath -> IO ()
processFile process infile outfile = 
  do 
    inf <- readFile infile
    writeFile outfile (process inf)
    
-- listBreak generalises break recursively. It takes a predicate and a list and breaks 
-- the list where the elements satisfy the predicates   
listBreak :: (a -> Bool) -> [a] -> [[a]]
listBreak pred [] = []
listBreak pred [x] = [[x]] 
listBreak pred (x0:x1:xs) = 
  if pred x1 then [x0]:(listBreak pred (x1:xs))
  else let (l1:ls) = listBreak pred (x1:xs) in ((x0:l1):ls)

-- begin with types for a line of input and a card
type Line = String
type Card = [Line]

-- lines beginning with "BEGIN:VCARD" denote the start of a new card. 
isBeginLine :: String -> Bool
isBeginLine line = take 11 line == "BEGIN:VCARD"

-- to convert string as input to list of cards
stringToCards :: [Line] -> [Card]  
stringToCards s = listBreak isBeginLine s

-- extract last name from card, 
-- take list of lines beginning "N:" (containing name)
-- if empty, there is no name and return empty list
-- otherwise extract name from first element of list
lastName :: Card -> String
lastName card = let nameLines = filter (\x -> take 2 x =="N:") card
                in case nameLines of 
                   []  -> ""
                   (nameLine:_) -> takeWhile (/= ';') (drop 2 nameLine)

-- extract last name from card,
-- as previous
firstNames :: Card -> String
firstNames card = let nameLines = filter (\x -> take 2 x =="N:") card 
                  in case nameLines of 
                   []  -> ""
                   (nameLine:_) -> takeWhile (/= ';') $ tail $ dropWhile (/= ';') $ drop 2 nameLine   
                  
-- generic function to extract phone number of a particular kind (HOME, CELL, WORK)
phoneType :: String -> Card -> String
phoneType kind  card = 
   let lines = filter (\x -> take (length kind) x == kind) $ 
                         map tail $ map (dropWhile (/= '=')) $ 
                          filter (\x -> take 3 x == "TEL") $ card
   in case lines of 
       [] -> ""
       (line:_) -> init $ tail $ dropWhile (/= ':') $ line                       

-- instances 
homePhone :: Card -> String
homePhone = phoneType "HOME"

cellPhone :: Card -> String
cellPhone = phoneType "CELL"

workPhone :: Card -> String
workPhone = phoneType "WORK"

-- first email address given on card
email :: Card -> String
email card = 
  let emailLines = filter (\x -> take 5 x == "EMAIL") $ card
  in case emailLines of 
       [] -> ""
       (emailLine:_) -> init $ tail $ dropWhile (/= ':') $ emailLine

-- extracts address as list of fields
address :: Card -> [String]
address card = 
  let addrLines = filter (\x -> take 3 x == "ADR") $ card
  in case addrLines of 
       [] -> []
       (addrLine:_) -> map tail $ listBreak (== ';') $ tail $ dropWhile (/= ':') $ addrLine

-- extracts individual address field from card
addressfield :: Int -> Card -> String
addressfield field card = 
  let addr = address card 
  in if (field <= length addr) then addr!!(field-1) else ""
  
-- converts a card to a line of csv
csvline :: Card -> String
csvline card = 
  (lastName card) ++ "," ++ (firstNames card) ++ ","
  ++ (homePhone card) ++ "," ++ (cellPhone card) ++ "," ++ (workPhone card) ++ ","
  ++ (email card) ++ ","
  ++ (addressfield 2 card) ++ "," ++ (addressfield 3 card) ++ "," ++ (addressfield 4 card) ++ "," ++ (addressfield 5 card) ++ ","
  
-- processes a String representing a list of cards to the contents of a csv file
-- processfile cardsToCSV "cards.vcf" "cards.csv" 
-- will now produce a csv file
cardsToCSV :: String -> String
cardsToCSV cards = unlines $ map csvline $ stringToCards $ lines cards  


{-
PART 3
-}

normalise :: String -> String
normalise phone =  "+44 "++ (tail $ filter (\x -> elem x ['0'..'9']) phone)

-- Just to get you using pattern matching

type Name = String
type Address = String
type Phone = String

data Card' = Card Name Address Phone

hasname :: String -> Card' -> Bool
hasname name (Card n _ _) = name == n

lookup :: String -> Card' -> Phone
lookup name (Card n _ p) = if (name==n) then p else error "wrong name"


