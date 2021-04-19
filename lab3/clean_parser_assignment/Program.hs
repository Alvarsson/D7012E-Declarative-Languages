module Program(T, parse, fromString, toString, exec) where
import Parser hiding (T)
import qualified Statement
import qualified Dictionary
import Prelude hiding (return, fail)
newtype T = Program [Statement.T] deriving (Show)-- to be defined
instance Parse T where
  parse = stateParse--error "Program.parse not implemented"
  toString = strShow--error "Program.toString not implemented"

{-| 
In the Program module you should represent the program as a Statement list.
 Use the parse function from the Statement module to define the parse function in this module.
  Use the exec function in the Statement module to execute a program.
-}

stateParse :: Parser T
stateParse = iter Statement.parse >-> stateParser
stateParser  = Program 

--creates a list from a list generating function by application of
  -- this function on all elements in a list passed as the second argument 
-- concat and map has this nice concatmap abrief.
strShow :: T -> String 
strShow (Program []) = []
strShow (Program statements) = concatMap Statement.toString statements



exec :: T -> [Integer] -> [Integer]
exec (Program prog) = Statement.exec prog Dictionary.empty--error "Program.exec not implemented"
