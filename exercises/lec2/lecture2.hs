
import System.IO

main = do
    print ("orderTriple", orderTriple (2,4,3))
    print("divisors", divisors 12)
    print("isPrime", isPrime 11)
    print("matches", matches 2 [3,2,4,5,2,2,5,6])
    print("elem1", elem1 5 [3,2,4,5,2,2,5,6])
    print("elem2", elem2 4 [3,2,4,5,2,2,5,6])
    print("onSeparateLines", onSeparateLines ["hej", "tjo", "bror"])
    print("duplicate", duplicate "Tjo" 4)
    print("pushRight", pushRight "Jatko" 5)
    print("patternMatch", patternMatch [1,2])
    print("rewrite", rewrite [1,2])
    print("product", productList [1,2,3,4])


    print("unique2", unique2 [2,3,4,2,3,5,4,1,3,6,3])
    print("isPalim", isPalim "naturrutan")
    print("isPalim2", isPalim2 "natur rutan")

--------------- 5.2 -------------
orderTriple :: (Int, Int, Int) -> (Int, Int, Int)
orderTriple (x, y, z) 
    | (x >= y) && (y >= z) = (x, y, z)
    | (x >= z) && (z >= y) = (x, z, y) 
    | (y >= x) && (x >= z) = (y, x, z)
    | (y >= z) && (z >= x) = (y, z, x)
    | (z >= y) && (y >= x) = (z, y, x)
    | (z >= x) && (x >= y) = (z, x, y)

--------------- 5.10 -------------
divisors :: Int -> [Int]
divisors x = [i | i <- [1..x], (x `mod` i) == 0]

isPrime :: Int -> Bool 
isPrime num 
    | divisors num == [1,num] = True 
    | otherwise = False

--------------- 5.11 -------------
matches :: Int -> [Int] -> [Int]
matches _ [] = []
matches x ys = [e | e <- ys, e == x] 
--matches x (y:ys) = (if x == y then [x] else []) ++ matches x  ys

elem1 :: Int -> [Int] -> Bool 
elem1 x list = matches x list /= [] 

elem2 :: Int -> [Int] -> Bool 
elem2 _ [] = False 
elem2 x (h:t) = x == h || elem2 x t

--------------- 5.18 -------------
-- Polymorphism with type variables instead of Ints duh
--------------- 5.22 -------------
onSeparateLines :: [String] -> String
onSeparateLines [] = []
onSeparateLines (x:xs) = x ++ "\n" ++ onSeparateLines xs

printingFuncy :: [String] -> IO ()
printingFuncy str = putStr (onSeparateLines str)
--------------- 5.23 -------------

duplicate :: String -> Int -> String 
duplicate str n = 
    if n <= 0 
        then "" 
            else str ++ duplicate str (n-1)

--------------- 5.24 -------------
pushRight :: String -> Int -> String 
pushRight str n  
    | n <= 0 = str
    | otherwise = " " ++ pushRight str (n-1) 

--------------- 6.29 -------------
--NAH

--------------- 7.2 -------------
patternMatch :: [Int] -> Int 
patternMatch (x1:x2:xs) = x1+x2
patternMatch (x1:xs) = x1
patternMatch [] = 0

--------------- 7.3 -------------
rewrite :: [Int] -> Int 
rewrite xs 
    | lengthList xs >= 2 = headList xs + tailList xs
    | lengthList xs == 1 = headList xs
    | lengthList xs <= 0 = 0

lengthList :: [a] -> Int
lengthList xs = length xs

headList :: [b] -> b
headList xs = head xs 

tailList :: [b] -> b
tailList = last

--------------- 7.4 -------------
productList :: [Int] -> Int 
productList [] = 1
productList xs = foldr (+) 0 xs 

--------------- 7.7 -------------?
unique2 :: [Int] -> [Int]
unique2 [] = []
unique2 (x:xs) = [x] ++ [z | z <- unique2 xs, z /= x]

--------------- 7.25 -------------
isPalim :: String -> Bool  -- A string is a list of chars [Char]
isPalim "" = True 
--isPalim x = length x == 1 
isPalim [_] = True 
isPalim (f:efter) = f == last efter && isPalim (init efter)

isPalim2 str = isPalim [c | c <- str, c /= ' ' ]


