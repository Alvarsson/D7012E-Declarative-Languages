-- integer: k > 0
-- list l containing n integers
-- The program should print the smallest k set of l.
import Data.List
import System.IO
import Data.String


{-| TESTS
smallestKset [1,2,3,4,5] 2
smallestKset [x*(-1)^x | x <- [1..100]] 15
smallestKset [24,-11,-34,42,-24,7,-19,21] 6
smallestKset [3,2,-4,3,2,-5,-2,2,3,-3,2,-5,6,-2,2,3] 8
-}

main = 
    smallestKset [x*(-1)^x | x <- [1..100]] 15
    --print (sumList [1,2,3,4,5,-6]) 
    --print (sumList2 [1,2,3,4,5,-6])

--PRINTING MADE EZ--------------------------------------------------------------
--Print string
printToString :: String -> IO ()
printToString str = putStr str

--Print a list
kSmallestPrint :: String -> String 
kSmallestPrint str = "size \t i \t j \t sublist \n" ++ str

--print result lines of each type size, i, j, and list
resultPrint :: [(Int, Int, Int, [Int])] -> String --Takes [Int: size, Int: i, Int: j, [Int]: sublist array with values]
resultPrint [] = [] --Empty list then done 
resultPrint (x:xs) =  subResult x ++ "\n" ++ resultPrint xs  --show convert value to string

subResult :: (Int, Int, Int,[Int]) -> String
--subResult [] = [] NO need for this.

subResult (size,i,j,list) = show size ++ "\t" ++ show i ++ "\t" ++ show j ++ "\t"++ show list

{-| Alternative with recursion
sumList :: [Int] -> Int -- function sumList takes integer array and outputs integer (the sum)
sumList [] = 0 -- Empty returns zero
sumList (x : xs) = x + sumList xs --recurse over list and sum up
-}
-------------------------------------------------------------------------------

{-| MAKESUBSET INFO FOR MYSELF (SANITY CHECK)

 "| x <- s"  this feeds x the values of s one at a time.
counter starts at 1 that is i
size, i, j, list

ex: [1,2,3,4]
size goes 4,3,2,1
val is [1,2,3,4] at start

first loop: val = 1, counter = 1
--> size = 1, i = 1, j = 1, list = [1]
val = 2
--> size = 3, i = 1, j = 2, list = [1,2]
val = 3
--> size = 6, i = 1, j = 3, list = [1,2,3]
val = 4
--> size = 10, i = 1, j = 4, list = [1,2,3,4]

second loop: val = 1, counter = 2 (val is now [1,2,3])
--> size = 2, i = 2, j = 2, list = [2]
val = 2
--> size = 5, i = 2, j = 3, list = [2,3]
val = 3
--> size = 9, i = 2, j = 4, list = [2,3,4]

third loop: val = 1, counter = 3 (val is now [1,2])
--> size = 3, i = 3, j = 3, list = [3]
val = 2
--> size = 7, i = 3, j = 4, list = [3,4]

fourth loo: val = 1, counter = 4 (val is now [1])
--> size = 4, i = 4, j = 4, list = [4]

-}
--{-| Alternative way i guess, the other was cleaner with recursion.
sumList :: [Int] -> Int 
sumList [] = 0
sumList xs = foldr (+) 0 xs
--}

makeSubsets :: [Int] -> Int -> [(Int, Int, Int, [Int])] -- Same thought as with result input
makeSubsets [] counter = []
makeSubsets (x:xs) counter = [ (sumList (take val (x:xs)), counter , (counter + (val-1)), take val (x:xs)) | val<-[1..size]] ++ (makeSubsets xs (counter+1))
    where 
        size = length (x:xs)

{-| 
--[1,2,3,4]
-- 0,0,[1]
--

makeSubs2 :: [Int] -> Int -> [(Int, Int, [Int])]
makeSubs2 (x:xs) count = [ count, (count (val-1)) , func2 list count (val-1) | val <- [1..size]] ++ makesubs2 xs count+1
    where 
        size = lenght (x:xs)

-}

--smallestKset should print out 
smallestKset :: [Int] -> Int -> IO ()
smallestKset [] number = error "The list is empty, invalid input"
smallestKset list number = printToString(kSmallestPrint(resultPrint( take number (sortLists (makeSubsets (list) 1)))))

--Function to sort subset lists. Sort for smallest "size" first
sortLists :: [(Int, Int, Int, [Int])] -> [(Int, Int, Int, [Int])] 
sortLists [] = []
sortLists (x:xs) = inList x (sortLists xs)


-- Placing elements in correct order toghether with recursive sortLists function
-- Guard: if first elements of list has size smaller than next elements, place it first.
    -- else place second element first and recurse call.
inList :: (Int, Int, Int, [Int]) -> [(Int, Int, Int, [Int])] -> [(Int, Int, Int, [Int])] 
inList (w,x,y,z) [] = [(w,x,y,z)]
inList (w,x,y,z) ((s,i,j,l):list) 
    | sumList z < sumList l = (w,x,y,z):((s,i,j,l):list)  
    | otherwise = (s,i,j,l):inList (w,x,y,z) list





