--Project Team Members
-- Chris Feth
-- Katelynn Thorn
-- Trevor Jones
-- Natalie Coppa

module Project where

import Prelude hiding (num)

prelude = undefined --library-level functions to be implemented after milestone (after refactoring/input/output/first-class functions are implemented)

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
         | Sub
         | Mul
         | Equ
         | LTE Cmd
         | GTE Cmd
         | Let Var Prog Prog
         | Ref Var
         | IfThen Prog Prog
         | WhileLp Stmt
   deriving (Eq, Show)

data Stmt
         = Set Cmd
         | While Cmd Stmt
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
cmd (LTE x) e s   = case cmd x e s of
                        Just ((I x1) : (I x2) : xs) -> Just ((B (x2 <= x1)) : (I x2) : xs)
                        _  -> Nothing
cmd (GTE x) e s   = case cmd x e s of
                        Just ((I x1) : (I x2) : xs) -> Just ((B (x1 <= x2)) : (I x2) : xs)
                        _  -> Nothing
cmd Add _ s       = case s of 
                        ((I i) : (I j) : s') -> Just ((I (i + j)) : s')
                        _                                   -> Nothing
cmd Sub _ s       = case s of 
                        ((I i) : (I j) : s') -> Just ((I (j - i)) : s')
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
cmd (Let x b v) env s    = case prog b env s of 
                                 Just (i : s') -> prog v (set x i env) s'
                                 Nothing                   -> Nothing
cmd (Ref x) env s        = case get x env of
                                 Just i -> Just (i : s)
                                 _      -> Nothing
cmd (WhileLp x) env s      = stmt x s

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

stmt :: Stmt -> Domain
stmt (Set e)     s = cmd e empty s
stmt (While c b)  s = case cmd c empty s of
                           Just ((B True) : s') -> case (stmt b s') of 
                                                      Just t -> stmt (While c b) t
                                                      _      -> Just s
                           Just ((B False) : s') -> Just s
                           _                     -> Nothing
stmt (Begin ss)  s = stmts ss s

stmts :: [Stmt] -> Domain
stmts [] s = Just s
stmts (x : xs) s = case stmt x s of
                         Just t -> stmts xs t
                         _      -> Nothing


nott :: Cmd
nott = IfThen [PushB False] [PushB True]



--EXAMPLE OF GOOD PROGRAMS

goodExample1 :: Prog
goodExample1 = [(WhileLp (Begin [Set (PushN 1900),While (GTE (PushN 10)) (Begin [(Set (PushN 400)), (Set (Sub))])])), PushN 0, Equ, (IfThen [PushS "Leap Year"] [PushS "Non-Leap Year"])]
-- THE ABOVE CHECKS WHETHER A GIVEN YEAR IS A LEAP YEAR. REPLACE "1900" WITH THE YEAR YOU WANT TO CHECK


--EXAMPLE OF BAD PROGRAMS
badExample1 :: Prog
badExample1 = [(PushS "test"), (PushN 5), Add]

badExample2 :: Prog
badExample2 = [PushN 5, Add]


