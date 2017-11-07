processFile :: (String -> String) -> FilePath -> FilePath -> IO ()
processFile process infile outfile = 
  do 
    inf <- readFile infile
    writeFile outfile (process inf)
    
   
listBreak :: (a -> Bool) -> [a] -> [[a]]
listBreak pred [] = []
listBreak pred [x] = [[x]] 
listBreak pred (x0:x1:xs) = 
  if pred x1 then [x0]:(listBreak pred (x1:xs))
  else let (l1:ls) = listBreak pred (x1:xs) in ((x0:l1):ls)
    
  