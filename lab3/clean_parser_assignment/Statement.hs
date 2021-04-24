module Statement(T, parse, toString, fromString, exec) where
import Prelude hiding (read,repeat, return, fail)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr

-- # : applies two parsers in sequence where the remainder string from the first one is fed into the other
-- -# : throws away result from first
-- #- : throws away result from second
-- ! : of (m ! n) which are of same type, create parser that applied m to input unless it fails, then apply n.

-- Q: which statements exactly?
-- Q: For the parser, should we do every step(exercise) from the appended material. Can it be done the same way as suggested?
-- Q: I dont really understand instance parse statement where

type T = Statement
data Statement =
    Assignment String Expr.T | -- variable and a value
    If Expr.T Statement Statement |
    While Expr.T Statement | -- 'while' expr 'do' statement
    Skip | --String?? semicolon or nothing?
    Begin [Statement] | -- List of statements. 'begin' statements 'end'
    Replicate Statement Expr.T | -- creates a list of length given by the first argument and the items having value of the second argument
    Read String | --Variable  'read' variable ';'
    Write Expr.T
    deriving Show

assignment :: Parser Statement 
assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss
buildAss (v, e) = Assignment v e

cond :: Parser Statement 
--accect if "if parsed expression from Expr" + require then "if parse, (and if parse throw away else)" else require else + parse  then return parser of condition type 
cond = accept "if" -# Expr.parse # require "then" -# parse #- require "else" # parse >-> condition
--condition takes an ((expression, statement), statement) and returns statement of If type.
condition ((exp,stmt1),stmt2) = If exp stmt1 stmt2


while :: Parser Statement
-- accept while if parsed ecpression from Expr 
while = accept "while" -# Expr.parse #- require "do" # parse >-> whileParser
--This should work
whileParser (i,j) = While i j 

skip :: Parser Statement
skip = accept "skip" #- require ";" >-> skipParser
skipParser i = Skip

begin :: Parser Statement
begin = accept "begin" -# iter parse #- require "end" >-> beginParser --
beginParser i = Begin i --only one statement to parse

repeat :: Parser Statement
repeat = accept "repeat" -# parse #- require "until" # Expr.parse #- require ";" >-> repParser
repParser (i, j) = Replicate i j

read :: Parser Statement --read a "word"
read = accept "read" -# word #- require ";" >-> readParser --can i use word to read whole string?
readParser w = Read w

write :: Parser Statement --write an expression
write = accept "write" -# Expr.parse #- require ";" >-> writeParser
writeParser i = Write i

{-|
 takes a list of statements to be executed, a dictionary containing variable/value pairs,
   and a list of integers containing numbers that may be read by read statements and the returned 
   list contains the numbers produced by write statements.
-}
exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]

exec (Assignment s e:statements) dic inp = 
    exec statements (Dictionary.insert (s, Expr.value e dic) dic) inp --hheres hoping this works. Insert value in dict.

exec (If cond thenStmts elseStmts: stmts) dict input = 
    if (Expr.value cond dict)>0 
    then exec (thenStmts: stmts) dict input
    else exec (elseStmts: stmts) dict input

-- need to do while statements after each statement then recurse over exec
exec (While e stmt:statements) dic inp =
    if (Expr.value e dic)>0
        then exec (stmt:While e stmt:statements) dic inp --if expression value larger than 0 then execute next while statement
            else exec (statements) dic inp-- else execute next statement

exec (Skip:statements) dic inp =
    exec statements dic inp

exec (Begin stmt:statements) dic inp = --
    exec (stmt ++ statements) dic inp -- Should it be stmt ++ statements?? or try with just statements

exec (Replicate e stmt:statements) dic inp = 
    exec (e : nextStatement) dic inp 
        where 
            nextStatement 
                | Expr.value stmt dic <= 0 = Replicate e stmt:statements
                | otherwise = statements
{-| 
    if (Expr.value e dic)>=0
        then exec (stmt:(Replicate e stmt):statements) dic inp -- SAME AS WHILE 
            else exec (statements) dic inp-- 
-}

exec (Read str:stmts) dic (inp:input) = -- Need to add it do dictionary?
    exec stmts (Dictionary.insert (str,inp) dic) input -- 

exec (Write e:statement) dic inp = -- will add a value to the returned list
    (Expr.value e dic):(exec statement dic inp)--testing with exp.valu---------------------

exec [] dic inp = []


instance Parse Statement where -- instance of Parser with type Statement
  parse = assignment ! cond  ! while ! begin ! repeat ! write ! skip ! read -- left associative will try to create parser untill success.
  toString = strShow --error "Statement.toString not implemented"

--Creating the strings from list of statements
-- How to show the statements and their extra functionality
strShow :: T -> String
strShow (Assignment v val) = v ++ 
    " := " ++
    Expr.toString val ++ 
    "\n" --Assignment

strShow (If exp stmt elseStmt) = "if " ++
    Expr.toString exp ++  --Is only expression to evaluate.
    " then\n" ++ 
    strShow stmt ++
    " else\n" ++
    strShow elseStmt

strShow (While exp stmt) = "while " ++ --practically same struct as if
    Expr.toString exp ++
    " do\n" ++
    strShow stmt 

strShow (Skip) = "skip;\n"

strShow (Begin stmts) = foldl (++) "begin\n"
    (map strShow stmts) ++
    " end\n"
    --"begin " ++ --foldl (from left) or should concatinate with "begin "?? Tests does not compute: "end near statement"
    --foldl (++) " " (map strShow stmts) ++
    --" end\n"
    

strShow (Replicate st exp) = "repeat\n" ++ --Should maybe set as replicate "statement" until "expression"... more readable
    strShow st ++
    "until " ++
    Expr.toString exp ++
    "\n" 

strShow (Read str) = "read " ++ -- only need to display the string given
    show str ++
    ";\n"

strShow (Write exp) = "write " ++
    Expr.toString exp ++
    ";\n"


