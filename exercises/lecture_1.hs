
main = 
    --print (threeDifferent 3 4 3)
    --print (threeDifferent2 3 4 3)
    print (fourEqual 4 4 3 4)

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
smallerRoot a b c = (-b - sqrt (b**2 - 4*a*c))/ (2*a)

largerRoot :: Float -> Float -> Float -> Float 
largerRoot a b c = (-b + sqrt (b**2 - 4*a*c))/ (2*a)