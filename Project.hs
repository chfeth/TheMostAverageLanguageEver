--Project Team Members
-- Chris Feth
-- Katelynn Thorn
-- Trevor Jones
-- Natalie Coppa

module Project where

import Prelude hiding (num)

prelude = undefined --library-level functions to be implemented after milestone (after refactoring/input/output/first-class functions are implemented)

--ABSTRACT SYNTAX----------------------
type Prog = [Cmd]

type Var = String

data Cmd = PushT Type
         | Rot
         | Drop
         | Dup
         | Swap
         | Over
         | Add
         | Sub
         | Mul
         | Equ
         | Mod
         | Div
         | LTE Cmd
         | GTE Cmd
         | Let Var Prog Prog
         | Ref Var
         | Def Var Prog
         | Call Var
         | CallStack
         | IfElse Prog Prog
         | IfThen Prog
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
   deriving (Eq, Show)

-----------------------------------------

type Stack = [Type]

type Domain = Stack -> Maybe Stack


--SEMANTICS
cmd :: Cmd -> Env -> Domain
cmd (PushT x) _ s = case x of 
                        (I i) -> Just ((I i) : s)
                        (B b) -> Just ((B b) : s)
                        (S s') -> Just ((S s') : s)
                        (P p) -> Just ((P p) : s)
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
                        ((I i) : (I j) : s') -> Just ((B (i == j)) : s')
                        ((B k) : (B l) : s') -> Just ((B (k == l)) : s')
                        _                                    -> Nothing
cmd Mod e s       = case s of 
                        ((I i) : (I j) : s') -> Just ((I (j `mod` i)) : s')
                        _                    -> Nothing
                        
cmd Div e s       = case s of 
                        ((I i) : (I j) : s') -> Just ((I (j `div` i)) : s')
                        _                    -> Nothing
cmd (IfElse t v ) env s  = case s of 
                                 ((B True) : s')  -> prog t env s'
                                 ((B False) : s') -> prog v env s'
                                 _                        -> Nothing
cmd (IfThen t) env s  = case s of 
                                 ((B True) : s')  -> prog t env s'
                                 ((B False): s')  -> Just s'
                                 _                        -> Nothing
cmd (Let x b v) env s    = case prog b env s of 
                                 Just (i : s') -> prog v (set x i env) s'
                                 Nothing                   -> Nothing
cmd (Ref x) env s        = case get x env of
                                 Just i -> Just (i : s)
                                 _      -> Nothing
cmd (Call x) env s       = case get x env of
                                 Just (P p) -> prog p env s
                                 _          -> Nothing
cmd CallStack env s      = case s of
                                 ((P p): s') -> prog p env s'
                                 _          -> Nothing
cmd (WhileLp x) env s      = stmt x env s

--EXECUTE LIST OF CMDS
prog :: Prog -> Env -> Domain
prog [] e s   = Just s
prog (c:p) e s = case c of 
                        (Def x y) -> (case set x (P y) e of
                                          e' -> prog p e' s)
                        _         -> (case cmd c e s of 
                                          Just s' -> prog p e s'
                                          _       -> Nothing)          


--NAMING/FUNCTION ENVIRONMENT
type Env = Var -> Maybe Type

empty :: Env
empty = \_ -> Nothing

get :: Var -> Env -> Maybe Type
get x m = m x

set :: Var -> Type -> Env -> Env
set x i m y = if y == x then Just i else m y -- instead of m y could be get y m


--WHILE LOOPS
stmt :: Stmt -> Env -> Domain
stmt (Set e) env s = cmd e env s
stmt (While c b) env  s = case cmd c env s of
                           Just ((B True) : s') -> case (stmt b env s') of 
                                                      Just t -> stmt (While c b) env t
                                                      _      -> Just s
                           Just ((B False) : s') -> Just s
                           _                     -> Nothing
stmt (Begin ss) env s = stmts ss env s

stmts :: [Stmt] -> Env -> Domain
stmts [] env s = Just s
stmts (x : xs) env s = case stmt x env s of
                         Just t -> stmts xs env t
                         _      -> Nothing


nott :: Cmd
nott = IfElse [PushT (B False)] [PushT (B True)]

--EXAMPLE OF GOOD PROGRAMS

goodExample1 :: Prog
goodExample1 = [PushT (I 1900), Dup, (WhileLp (While (GTE (PushT (I 1))) (Begin [(Set (PushT (I 4))), (Set (Sub))]))), -- push year and while loop sub 4 to check if year is divisible by 4
                -- this next line checks the output of the while loop, and checks if the year is divisible by 100, and if it is if it is also divisible by 400, and pushes the correct leapyear/non-leapyear text to stack
                Swap, Dup, Rot,                           -- duplicate year so we have it for later checks
                PushT (I 0), Equ,                         -- check outcome from previous loop to see if year is divisible by 4
                -- if it is divisible by 4, then also check 100 and 400, as 100 only years are not leap years, but 400 years are leap years.
                (IfElse [PushT (I 100), Mod, PushT (I 0), Equ, IfElse [PushT (I 400), Mod, PushT(I 0), Equ, IfElse [PushT (S "Leap Year")] [PushT (S "Non-Leap Year")]] [PushT (S "Leap Year")]] [PushT (S "Non-Leap Year")])]
-- THE ABOVE CHECKS WHETHER A GIVEN YEAR IS A LEAP YEAR. REPLACE "1900" WITH THE YEAR YOU WANT TO CHECK

-- This example takes a 13 digit isbn and checks it's check bit to make sure it is a valid isbn number
-- https://isbn-information.com/check-digit-for-the-13-digit-isbn.html
-- Example isbn to use:
-- 9783161484100 - valid
-- 9781861972712 - valid
-- 9781681972712 - invalid
-- 9781861973712 - invalid
-- top value of stack is either true or false based on validty of isbn after running
goodExample2 :: Prog
goodExample2 = [(Def "SplitInt" [Dup, (PushT (I 10)), Div, Swap, (PushT (I 10)), Mod, Swap]),    -- function to split integer into digits
                (PushT (I 9783161484100)),                                                       -- isbn we want to use REMOVE THIS LINE IF YOU WANT TO PASS IN YOUR OWN ISBN ON THE STACK
                (Call "SplitInt"), (PushT (I (-1))), Swap,                                       -- do the  first call and push a -1 so we know when the first 12 digits end
                (WhileLp (While (GTE (PushT (I 10))) (Begin [Set(Call "SplitInt")]))),           -- while loop to call SplitInt until isbn number is split into single digits
                (Def "FlipOneThree" [(GTE (PushT (I 3))), IfElse [Drop, PushT (I 1)] [Drop, PushT (I 3)]]),    -- function that swaps the first number on the stack between 1 or 3
                (PushT (I 1)), (PushT (I 0)), Rot,                                               -- setup for summing number, 1 is what we multiply by, 0 is the starting sum
                -- while loop that loops through and multiplies the first num by 1, then the next num by 3, and adds them, then multiplies the third num by 1, and adds to sum.
                -- it continues through all numbers, alternating between 1 and 3 by pushing the FlipOneThree function to the stack and calling it, until it sums all 12 digits.
                (WhileLp (While (GTE (PushT (I 0))) (Begin [Set(Swap), Set(Dup), Set(Rot), Set(Mul), Set(Swap), Set(Ref "FlipOneThree"), Set(CallStack), Set(Rot), Set(Add), Set(Rot)]))),
                -- drop the 1or3 counter and -1 marker, then mod 10 to get remainder, and subtract from 10 if not 0, then compare to 13th check bit in isbn
                Drop, Drop, (PushT (I 10)), Mod, (GTE (PushT (I 1))), IfThen [PushT (I 10), Swap, Sub], Equ]


funcExample1 :: Prog
funcExample1 = [(PushT (I 25)), (PushT(P [(PushT (I 5)), Add, (PushT (I (-1))), Mul])), CallStack]
-- THE ABOVE PUSHES 25, THEN DEFINES A FUNCTION THAT ADDS 5 AND FLIPS THE SIGN, AND PUSHES THAT FUNCTION TO THE STACK. IT RUNS IT OFF THE STACK, RESULTING IN -30

--EXAMPLE OF BAD PROGRAMS
badExample1 :: Prog
badExample1 = [(PushT (S "test")), (PushT (I 5)), Add]

badExample2 :: Prog
badExample2 = [PushT (I 5), Add]
