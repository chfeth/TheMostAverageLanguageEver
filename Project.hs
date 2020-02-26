--Project Team Members
-- Chris Feth
-- Katelynn Thorn
-- Trevor Jones
-- Natalie Coppa

module Project where

import Prelude hiding (num)

type Prog = [Cmd]

type Var = String

data Cmd = PushN Int
         | PushB Bool
         | PushS String
         | Rot
         | Drop
         | Dup
         | Swap
         | Over
         | Add
         | Mul
         | Equ
         | Let Var Cmd Cmd
         | Ref Var
         | IfThen Prog Prog
   deriving (Eq, Show)

data Stmt
         = Set Cmd
         | While Test Stmt
         | Begin [Stmt]
  deriving (Eq,Show)

data Type
         = I Int
         | B Bool
         | S String
         | P Prog
         | Error
   deriving (Eq, Show)

type Stack = [Type]

type Domain = Stack -> Maybe Stack

cmd :: Cmd -> Env -> Domain
cmd (PushN i) _ s = Just ((I i) : s)
cmd (PushB b) _ s = Just ((B b) : s)
cmd (PushS str) _ s = Just ((S str) : s)
cmd (Rot) _ s     = case s of 
                        (x1 : x2 : x3 : xs) -> Just (x3 : x2 : x1 : xs)
                        _        -> Nothing
cmd (Drop) _ s    = case s of 
                        (x : xs) -> Just xs
                        _        -> Nothing
cmd (Dup) _ s     = case s of 
                        (x : xs) -> Just (x : x : xs)
                        _        -> Nothing
cmd (Swap) _ s    = case s of 
                        (x1 : x2 : xs) -> Just (x2 : x1 : xs)
                        _              -> Nothing
cmd (Over) _ s    = case s of 
                        (x1 : x2 : xs) -> Just (x2 : x1 : x2 : xs)
                        _              -> Nothing
cmd Add _ s       = case s of 
                        ((I i) : (I j) : s') -> Just ((I (i + j)) : s')
                        _                                   -> Nothing
cmd Mul e s       = case s of 
                        ((I i) : (I j) : s') -> Just ((I (i * j)) : s')
                        _                                  -> Nothing
cmd Equ e s       = case s of 
                        ((I i) : (I j) : s')   -> Just ((B (i == j)) : s')
                        ((B k) : (B l) : s') -> Just ((B (k == l)) : s')
                        _                                    -> Nothing
cmd (IfThen t v ) env s  = case s of 
                                 ((B True) : s')  -> prog t env s'
                                 ((B False) : s') -> prog v env s'
                                 _                        -> Nothing
cmd (Let x b v) env s    = case cmd b env s of 
                                 Just (i : s') -> cmd v (set x i env) s'
                                 Nothing                   -> Nothing
cmd (Ref x) env s        = case get x env of
                                 Just i -> Just (i : s)
                                 _      -> Nothing

prog :: Prog -> Env -> Domain
prog [] e s   = Just s
prog (c:p) e s = case cmd c e s of 
                            Just s' -> prog p e s'
                            _       -> Nothing


type Env = Var -> Maybe Type

empty :: Env
empty = \_ -> Nothing

get :: Var -> Env -> Maybe Type
get x m = m x

set :: Var -> Type -> Env -> Env
set x i m y = if y == x then Just i else m y -- instead of m y could be get y m

stmt :: Stmt -> Type -> Domain
stmt (Set e)    t s = cmd e s
stmt (While c b) t s = if test c s then stmt (While c b) (stmt b s) else s
stmt (Begin ss) t s = stmts ss s  -- foldl (flip stmt) s ss
  where
    stmts []     r = r
    stmts (s:ss) r = stmts ss (stmt s r)

