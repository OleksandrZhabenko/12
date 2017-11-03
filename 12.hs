{-
MIT License

Copyright (c) 2017 OleksandrZhabenko

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
-}

isComma :: Char -> Bool
isComma c = if c == ',' then True else False

divideString :: (Char -> Bool) -> String -> [String]
divideString p [] = [""]
divideString p x = if (foldr1 (||) (map p x)) 
   then map (fst . (break p)) (take (y+1) (iterate (tail . (dropWhile (not . p))) x))
   else [x]   
      where y = (length $ filter p x)

cells :: String -> [[String]]
cells x = map (divideString isComma) (lines x)

changeCell :: String -> String -> String -> String
changeCell x y z | (y /= []) = y
                 | (y == []) && (z /= []) = x
                 | otherwise = ""

isTruncated :: String -> String -> Bool
isTruncated w w' = if ((null w) && (not $ null w')) then True else False

boolListTruncated :: [String] -> [Bool]
boolListTruncated y = zipWith isTruncated y y''
   where y'' = "":y
   
countChanged :: [Bool] -> Int
countChanged z = length (takeWhile (== False) z)

createSecondLine :: [String] -> [String] -> [String]
createSecondLine x y = take (countChanged (boolListTruncated y)) y'
   where y' = zipWith3 changeCell x y y''
         y'' = (tail y) ++ [""]
            
lineN :: Int -> [[String]] -> [String]
lineN n x = last $ (take n x)

lineN' :: Int -> [[String]] -> [String]
lineN' n x = last $ (take (n-1) x)

createNthLine :: [[String]] -> Int -> [String]
createNthLine x@(xs:xss) n | ((n < 1) || (n > length x)) = error "Undefined line!"
                           | n == 1 = xs
                           | otherwise = createSecondLine (lineN' n x) (lineN n x)
                           
fillEmptyCells :: [[String]] -> [[String]]
fillEmptyCells x = map (createNthLine x) [1..(length x)]

changeNthLine :: [String] -> String
changeNthLine x = "\"" ++ (concatMap (++"\"->\"") x) ++ "\n"

dropLast :: String -> String
dropLast x | ((drop (length x - 4) x) == "->\"\n") = dropLast (take (length x - 4) x)
           | otherwise = x ++ "\n"
           
dropDouble :: String -> String
dropDouble x = if length x >= 2 
               then if (head x == '\"') && (head (tail x) == '\"') 
                    then dropDouble (tail x)
                    else (head x):(dropDouble (tail x))
               else x
                  
dropNull :: [String] -> [String]
dropNull = filter (not . null)

processCellsBeginning :: String -> [[String]]
processCellsBeginning = fillEmptyCells . cells

processCellsEnd :: [[String]] -> String
processCellsEnd xss = concat ((map (dropDouble . dropLast . changeNthLine) xss))

takeColumn :: Int -> [[String]] -> [String]
takeColumn n x | ((n < 1) || (n > length (head x))) = error "Undefined line!"
               | otherwise = map (head . (drop (n-1) )) x
                 
findLastX :: Int -> Int -> [[String]] -> String
findLastX n m x | (m < 2) || (m > length x) = error "Undefined column!"
                | otherwise = last (filter (not . null) (take (m - 1) (takeColumn n x)))

createNthLine2 :: [[String]] -> Int -> [String]
createNthLine2 x@(xs:xss) n | ((n < 1) || (n > length x)) = error "Undefined line!"
                            | n == 1 = dropNull xs
                            | otherwise = if (null (head (lineN n x))) 
                                          then (findLastX (length (takeWhile null (lineN n x))) n x):(dropNull (lineN n x))
                                          else dropNull (lineN n x)
                                         
fillEmptyCells2 :: [[String]] -> [[String]]
fillEmptyCells2 x = map (createNthLine2 x) (reverse [1..(length x)])

beginsWithAtSign :: String -> Bool
beginsWithAtSign [] = False
beginsWithAtSign [x] = False
beginsWithAtSign x@(x':x'':xs) = if ((x' == '@') || ((x' == '\"') && (x'' == '@'))) then True else False

findFilledWithColor :: [[String]] -> [String]
findFilledWithColor x = (concat . map (filter (beginsWithAtSign) )) x

(+++) :: String -> String -> String -> String
(+++) x y z = x ++ y ++ z 

makeFilledWithColor :: [String] -> String
makeFilledWithColor x = concat (zipWith3 (+++) (repeat "\"") x  (repeat "\" [style=filled, fillcolor=\"#ffffba\"];\n"))

processCells :: String -> String
processCells x = processCellsEnd (fillEmptyCells2 (processCellsBeginning x))

combineCells :: String -> String
combineCells x = let (y,z) = ((fillEmptyCells2 (processCellsBeginning x)),(processCellsEnd (fillEmptyCells2 (processCellsBeginning x)))) in concat ["strict digraph 1 {\n","overlap=false\n",z,(makeFilledWithColor (findFilledWithColor y)),"\n}\n"]

main = readFile "1.csv" >>= return . combineCells >>= writeFile "x12.gv"
