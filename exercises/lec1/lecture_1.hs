
main = 
    --print (threeDifferent 3 4 3)
    --print (threeDifferent2 3 4 3)
    --print (fourEqual 4 4 3 4)
    --print(largestSquare 15 1)
    print (findMax 4)

--------------- 3.7 -------------
-- GUARD
threeDifferent :: Int -> Int -> Int -> Bool 
threeDifferent m n p
    | m /= n && m /= p && n /= p = True 
    | otherwise = False

-- IF
threeDifferent2 :: Int -> Int -> Int -> Bool 
threeDifferent2 m n p = if m /= n && m /= p && n /= p then True else False


--------------- 3.8 -------------

twoEqual :: Int -> Int -> Bool 
twoEqual w x = w == x

threeEqual :: Int -> Int -> Int -> Bool 
threeEqual w x y =  twoEqual w x == twoEqual x y 

fourEqual :: Int -> Int -> Int -> Int -> Bool 
fourEqual w x y z = threeEqual w x y == threeEqual x y z

--------------- 3.17 -------------

smallerRoot :: Float -> Float -> Float -> Float  
smallerRoot a b c = (-b - sqrt (b^2 - 4*a*c))/ (2*a)

largerRoot :: Float -> Float -> Float -> Float 
largerRoot a b c = (-b + sqrt (b^2 - 4*a*c))/ (2*a)


--------------- 4.7 -------------
mult :: Int -> Int -> Int 
mult m n = 
    if n == 0 then 0
    else m + mult m (n-1)

--------------- 4.8 -------------
largestSquare :: Int -> Int -> Int
largestSquare x counter
    | counter^2> x = counter - 1
    | otherwise  = largestSquare x (counter+1)

--------------- 4.9 -------------
f :: Int -> Int  
f 0 = 0
f 1 = 44
f 2 = 17
f 3 = 45
f 4 = 50 

findMax :: Int -> Int 
findMax x = maximum (createList x)

createList :: Int -> [Int]
createList n = [f i | i <- [1..n] ]

--------------- 4.14 -------------
{-|  BAD DESCRIPTION WONT DO
 powerOfTwo :: Int -> Int 
 powerOfTwo n 
    | (n `mod` 2 == 0) = 2 * powerOfTwo (n)--EVEN
    | 

-}


