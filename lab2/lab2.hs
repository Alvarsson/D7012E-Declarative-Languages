-- Code to Haskell lab assignment 2 in the course D7012E by HÃ¥kan Jonsson

import Data.Char

main :: IO ()
main = do 
    print(parse "10")
    print(parse "x")
    print(parse "10+x")
    print(parse "1+2*(3-4/5)")
    print(parse "sqrt(1+sin(x))")
    -- testing some stuff
    print "--------testing for part one-------------"
    print(unparse (simplify (diff (Var "x") (parse "exp(sin(2*x))"))))
    print(unparse (simplify (diff (Var "x") (parse "sin(2*x)"))))
    print(unparse (simplify (diff (Var "x") (parse "cos(2*x)"))))
    print(unparse (simplify (diff (Var "x") (parse "log(x)"))))
    print "--------testing for part two-------------"
    print(mkfun (parse "x*x+2", Var "x") 3)
    print(mkfun (parse "x+7", Var "x") 1330)
    print "--------testing for part three-------------"
    print(findzero "x" "x*x*x+x-1" 1.0) --0.68232775
    print(findzero "y" "cos(y)*sin(y)" 2.0) --1.5707964
    print(findzero "x" "x-2" 1.0)
    print "--------testing for TESTING-------------"
    --print(eval (diff (parse "cos(y)*sin(y)") (parse "y")) [("y", 2.0)])
    --print(unparse (simplify (diff (Var "x") (parse "cos(2*x)*sin(x)"))))


--EXPR::
-- integer constants, names of variables, binary operators with two operands of type
--EXPR, and function applications where a function is applied on a value of type EXPR.
-- The two last kinds of values are tree nodes.
data EXPR = Const Int
     | Var String
     | Op String EXPR EXPR
     | App String EXPR deriving (Eq, Ord, Show)

--parse::
-- parse interprets an expression in a string as a value of type EXPR.
-- We say that it parses the string.

parse :: String -> EXPR
parse = fst . buildexpr
  where
    notfirst p (_,[]) = True
    notfirst p (_,x:xs) = not (p x)
    
    buildnumber :: String -> (EXPR,String)
    buildnumber xs = until (notfirst isDigit) accdigits (Const 0, xs)
      where
        accdigits :: (EXPR,String) -> (EXPR,String)
        accdigits (Const n, y:ys) = (Const(10*n+(ord y - 48)), ys)
    
    buildvar :: String -> (EXPR,String)
    buildvar xs = until (notfirst isLetter) accletters (Var "", xs)
      where
        accletters :: (EXPR,String) -> (EXPR,String)
        accletters (Var s, y:ys) = (Var (s ++[y]), ys)
    
    
    buildexpr :: String -> (EXPR,String)
    buildexpr xs = until (notfirst (\c -> c=='-' || c=='+')) accterms (buildterm xs)
      where
        accterms :: (EXPR,String) -> (EXPR,String)
        accterms (term, y:ys) = (Op (y:[]) term term1, zs)
          where
            (term1,zs) = buildterm ys
    
    buildterm :: String -> (EXPR,String)
    buildterm xs = until (notfirst (\c -> c=='*' || c=='/')) accfactors (buildfactor xs)
      where
        accfactors :: (EXPR,String) -> (EXPR,String)  
        accfactors (fact, y:ys) = (Op (y:[]) fact fact1, zs)
          where
            (fact1,zs) = buildfactor ys
    
    buildfactor :: String -> (EXPR,String)
    buildfactor [] = error "missing factor"
    buildfactor ('(':xs) =  case buildexpr xs of (e, ')':ws) -> (e, ws); _ -> error "missing factor"
    buildfactor (x:xs)
      | isDigit x = buildnumber (x:xs)
      | isLetter x = case buildvar (x:xs) of
                       (Var s, '(':zs) -> let (e,ws)=buildfactor ('(':zs) in (App s e,ws)
                       p -> p
      | otherwise = error "illegal symbol"

--unparse::
--unparse turns an EXPR into a string.

unparse :: EXPR -> String
unparse (Const n) = show n
unparse (Var s) = s
unparse (Op oper e1 e2) = "(" ++ unparse e1 ++ oper ++ unparse e2 ++ ")"
-------------------------------------------------------------------------ADDED
--Need an unparse for function Application ::: App String EXPR deriving (Eq, Ord, Show)
unparse (App func e) = "(" ++ func ++ unparse e ++ ")"


--eval::
--eval evaluates the value of an EXPR. Note that we can only write integers in our
-- strings but eval still returns a floating-point number.

eval :: EXPR -> [(String,Float)] -> Float
eval (Const n) _ = fromIntegral n
eval (Var x) env = case lookup x env of Just y -> y ; _ -> error (x ++ " undefined")
eval (Op "+" left right) env = eval left env + eval right env
eval (Op "-" left right) env = eval left env - eval right env
eval (Op "*" left right) env = eval left env * eval right env
eval (Op "/" left right) env = eval left env / eval right env
-------------------------------------------------------------------------ADDED
-- eval cos
eval (App "cos" right) env = cos(eval right env)
-- eval sin
eval (App "sin" right) env = sin(eval right env)
-- eval log
eval (App "log" right) env = log(eval right env)
-- eval exp
eval (App "exp" right) env = exp(eval right env)


--diff::
--diff differentiates an EXPR symbolically with respect to a variable.
-- The derivative is also an EXPR.
 -- derivate the second EXPR with regards to first EXPR. ex 'cos(x)' is second EXPR, 'x' is first
diff :: EXPR -> EXPR -> EXPR
diff _ (Const _) = Const 0
diff (Var id) (Var id2)
  | id == id2 = Const 1
  | otherwise = Const 0
diff v (Op "+" e1 e2) = Op "+" (diff v e1) (diff v e2)
diff v (Op "-" e1 e2) = Op "-" (diff v e1) (diff v e2)
diff v (Op "*" e1 e2) =
  Op "+" (Op "*" (diff v e1) e2) (Op "*" e1 (diff v e2))
diff v (Op "/" e1 e2) =
  Op "/" (Op "-" (Op "*" (diff v e1) e1) (Op "*" e1 (diff v e2))) (Op "*" e2 e2)
-- diff cos
diff v (App "cos" right) = Op "*" (Op "-" (Const 0) (Const 1)) (Op "*" (diff v right) (App "sin" right))-- cos(ax) -> -asin(ax)
--diff v (App "cos" right) = "-" cant do this since wont always be negative.
-- diff sin
diff v (App "sin" right) = Op "*" (diff v right) (App "cos" right)  -- sin(ax) -> acos(ax)
--diff log 
diff v (App "log" right) = Op "/" (diff v right) right 
--diff exp
diff v (App "exp" right) = Op "*" (diff v right) (App "exp" right) 
diff _ _ = error "can not compute the derivative"


--simplify::
--simplify makes an EXPR simpler by applying a number of simplification rules.

simplify :: EXPR -> EXPR
simplify (Const n) = Const n
simplify (Var id) = Var id
simplify (Op oper left right) =
  let (lefts,rights) = (simplify left, simplify right) in
    case (oper, lefts, rights) of
      ("+",e,Const 0) -> e
      ("+",Const 0,e) -> e
      ("*",e,Const 0) -> Const 0
      ("*",Const 0,e) -> Const 0
      ("*",e,Const 1) -> e
      ("*",Const 1,e) -> e
      ("-",e,Const 0) -> e
      ("/",e,Const 1) -> e
      ("-",le,re)     -> if left==right then Const 0 else Op "-" le re
      (op,le,re)      -> Op op le re
simplify (App func expression) = App func (simplify expression) 


--print(mkfun (parse "x*x+2", Var "x") 3)
mkfun :: (EXPR, EXPR) -> (Float -> Float)
--mkfun (funcBody, Var funcVar) = \arg -> case arg of
--    [_, []] -> "error"
--    [x, arg] -> eval func 
mkfun (funcBody, Var funcVar) arg = eval funcBody [(unparse (Var funcVar), arg)] -- Send in argument (EXPR) contin. if large expression

-- Newton raphson method x1 = x0 - f(x0)/f'(x0)
-- xn-1 = xn - f(xn)/f'(xn)
--FIRST define newton raphson method recursively approximation towards thr (gränsvärde)
nRaphson ::  (Float -> Float) -> (Float -> Float) -> Float -> Float 
nRaphson func func' x
    | abs (x - x0) <= thr = x0
    | otherwise = nRaphson func func' x0
        where
            thr = 0.0001
            x0 = x  - ((func x) / (func' x) ) 

-- s1: name of unknown variable
-- s2: body of a function
-- from string create functions and use nRaphson to approximate the deriv value.
findzero :: String -> String -> Float -> Float 
findzero s1 s2 = nRaphson func func' --funcVar func func'
    where
        func = mkfun (parse s2, funcVar) -- send to make func with 
        func' = mkfun (diff funcVar (parse s2), funcVar)
        funcVar = parse s1 -- var such as x
{-|      

-- QUESTION: is it bad practice to simply put the entierty of the nRaphson method in a where clause?
findzero s1 s2 = NR funcVar func func' 
    where
        func = mkfun (parse s2, funcVar) -- send to make func with 
        func' = mkfun (diff funcVar (parse s2), funcVar)
        funcVar = parse s1 -- var such as x
        NR funcVar func func' x0  
            | abs .... 
-}










