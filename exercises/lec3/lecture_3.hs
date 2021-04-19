import System.IO

main = do
    print("isInc", isInc (\x -> x-1) 5 )
    print("sqrtMap", sumSqrsMap 3)
    print("sqrtFoldr", sumSqrsFoldr 3)



--------------- 9.2 -------------
lenght :: [a] -> Int 
lenght xs = sum (map (\x -> 1) xs)


len :: [a] -> Int 
len lst = sum x 
    where x = map justGiveMeOne lst

justGiveMeOne :: a -> Int  --bättre att göra detta till lokal funktion??
justGiveMeOne _ = 1

length1 :: [Int] -> Int 
lenght1[] = 0
length1 ls = sum $ map (\x -> x-x+1) ls-- sim appliceras på vad som beräknas på höger sida

-- Ex "(\x -> x+1) 6" kommer returnera ut 7
lenght2 :: [Int] -> Int 
lenght2 [] = 0
lenght2 ls = foldr (+) 0 $ map (\x -> x-x+1) ls-- backslash betyder function, här kommer en namnlös funktion

len2 :: [a] -> Int
len2 xs = foldr (+) 0 (map (\_ -> 1) xs) 

len2b :: [k] -> Int 
len2b = foldr (+) 0 . map (\_ -> 1) 
--måste veta map, foldr och...
-- foldr är en högre ordningens funktion
--foldr (+) 0 [1,2,3,4]-- noll är basfallet, (+) är funktionstypet så addition sker, [Int listan] är
-- datan som foldr jobbar mot 


--------------- 9.6 -------------
sqrs :: [Int] -> [Int]
sqrs lst = map (^2) lst

sumSqrs :: [Int] -> Int 
sumSqrs s = sum (sqrs s)

greater :: [Int] -> Bool 
greater g = length ls == lenght g 
    where ls = filter (>0) g
     
--------------- 9.7 -------------

minFunc :: (Int -> Int) -> Int -> Int 
minFunc f 0 = f 0
minFunc f n 
    | b < a = b
    | otherwise = a
        where 
            a = f n 
            b = minFunc f (n-1)

allEq :: (Int -> Int) -> Int -> Bool 
allEq _ 0 = True 
allEq f n 
    | f n == f (n-1) = allEq f (n-1)
    | otherwise = False

grtrZeroFunc :: (Int -> Int) -> Int -> Bool 
grtrZeroFunc _ (-1) = True 
grtrZeroFunc f n 
    | f n <= 0 = False
    | otherwise = grtrZeroFunc f (n-1)

isInc :: (Int -> Int) -> Int -> Bool 
isInc _ 0 = True 
isInc f n 
    | a > b = isInc f (n-1)
    | otherwise = False
    where
        a = f n 
        b = f (n-1)


--------------- 9.9 -------------
{-|
iter :: Int -> (Int -> Int) -> Int
iter 0 f = f 0
iter 
-}

--------------- 9.11 -------------

sumSqrsMap :: Int -> Int 
sumSqrsMap 0 = 0
sumSqrsMap n = sum (map (^2) [1..n])

sumSqrsFoldr :: Int -> Int 
sumSqrsFoldr n = foldr (\x y -> x*x+y) 0 [1..n]

-- [1,2,3]
-- 3 + 3 = 6
-- 2 + 6 = 8
-- 1 + 8 = 9

-- 3 * 3 = 9
-- 2 * 9 = 18
-- 1 * 18 = 18...

-- 3 *(3-3) = 0
-- 2 *(2-0) = 4
-- 1 *(1-4) = -3

-- 3 * 3 = 9
-- 9 +
--------------- 9.16 -------------

filterFirst :: (a -> Bool) -> [a] -> [a]
filterFirst _ [] = []
filterFirst p (x:xs) 
    | p x = x:filterFirst p xs --if p x is True then x is passed to start of list then recurse onwards
    | otherwise = xs -- p x did not hold and thus pass the rest (xs) and x is dropped

--------------- 9.17 -------------

filterLast :: (a -> Bool) -> [a] -> [a]
filterLast p xs = filterFirst p (reverse xs)


--------------- 10.3 -------------

composeList :: [a -> a] -> (a -> a) 
composeList (x:xs) = foldr (.) x xs

--version 2
composeList1 :: [a -> a] -> (a -> a)
composeList1 [] = id
composeList1 (x:xs) = x . composeList xs

------------- 10.7 -------------

flip1 :: (a -> b -> c) -> (b -> a -> c)
flip1 x y z = x z y

------------- 10.8 -------------

not1 :: Bool -> Bool
not1 b 
    | b = False 
    | otherwise = True

elem1 :: Eq t => t -> [t] -> Bool
elem1 _ [] = False 
elem1 n (y:xs) 
    | (\x -> y==n) xs = True
    | otherwise = elem1 n xs
