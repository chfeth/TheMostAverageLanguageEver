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


type Stack = [Either NumT Prog]

type NumT = Either Int Bool

type Domain = Stack -> Maybe Stack

cmd :: Cmd -> Env -> Domain
cmd (PushN i) _ s = Just (Left (Left i) : s)
cmd (PushB b) _ s = Just (Left (Right b) : s)
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
                        (Left(Left  i) : Left(Left j) : s') -> Just (Left(Left (i + j)) : s')
                        _                                   -> Nothing
cmd Mul e s       = case s of 
                        (Left(Left i) : Left(Left j) : s') -> Just (Left(Left (i * j)) : s')
                        _                                  -> Nothing
cmd Equ e s       = case s of 
                        (Left(Left i) : Left(Left j) : s')   -> Just (Left(Right (i == j)) : s')
                        (Left(Right k) : Left(Right l) : s') -> Just (Left(Right (k == l)) : s')
                        _                                    -> Nothing
cmd (IfThen t v ) env s  = case s of 
                                 (Left(Right True) : s')  -> prog t env s'
                                 (Left(Right False) : s') -> prog v env s'
                                 _                        -> Nothing
cmd (Let x b v) env s    = case cmd b env s of 
                                 Just (Left (Left i) : s') -> cmd v (set x i env) s'
                                 Nothing                   -> Nothing
cmd (Ref x) env s        = case get x env of
                                 Just i -> Just (Left (Left i) : s)
                                 _      -> Nothing

prog :: Prog -> Env -> Domain
prog [] e s   = Just s
prog (c:p) e s = case cmd c e s of 
                            Just s' -> prog p e s'
                            _       -> Nothing


type Env = Var -> Maybe Int

empty :: Env
empty = \_ -> Nothing

get :: Var -> Env -> Maybe Int
get x m = m x

set :: Var -> Int -> Env -> Env
set x i m y = if y == x then Just i else m y -- instead of m y could be get y m


