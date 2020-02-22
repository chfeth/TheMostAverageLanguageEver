--Project Team Members
-- Chris Feth
-- Katelynn Thorn
-- Trevor Jones
-- Natalie Coppa

module Project where

import <prelude> hiding (num)

-- Grammar for TheMostAverageLanguageEver
-- num ::= (any integer) 
-- var ::= (any variable name)
-- def ::= varName '=' [Int or Bool]
--       | functionName [Expr] [Expr]

-- Expr ::= num
--		  | bool
--        | 'add' expr expr
--        | `mul` expr expr                  
--        | `equ` expr expr  
--		  | 'Let' def '=' expr 'in' expr
--		  | 'Ref' def                    
--        | `if` def `else` def `end` 

-- Now we encode the above grammar as a set of Haskell data types

type Num = Int
type Var = String
type FunctionName = String

type Prog = [Expr]

data Def = Variable Var [Either Int Bool]
         | Function FunctionName [Expr] [Expr]
       deriving (Eq, Show)

data Expr = Lit Num
          | B Bool
          | Add Expr Expr
          | Mul Expr Expr 
          | Equ Expr Expr
          | Let Var Expr Expr
          | Ref Var 
          | IfThen Prog Prog
       deriving (Eq, Show)

data Stmt = Set Expr
          | While Expr Stmt
          | Begin [Stmt]
       deriving (Eq, Show)

-- Good Programs ------------------------------------

-- Ex 1 Concrete Syntax
-- [Function goodFunction [Lit 5, ] []]
-- Ex 2 Concrete Syntax

-- Bad Programs -------------------------------------



-----------------------------------------------------
-- type Envr = Var -> Maybe Expr

-- empty :: Env
-- empty = \_ -> Nothing

-- get :: Var -> Env -> Maybe Expr

-- set :: Var -> Expr -> Env -> Env

-- expr :: Expr -> Env -> 





