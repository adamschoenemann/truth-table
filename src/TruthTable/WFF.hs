module TruthTable.WFF (
      WFF(..), (∧), (∨), (-->), (<->), (/\), (\/)
    , eval, traverse, enumerate
) where

import qualified  Data.Map as Map
import Data.List (nub)

data WFF = Const Bool
         | Var String
         | Not  WFF
         | And  WFF WFF
         | Or   WFF WFF
         | Impl WFF WFF
         | Equi WFF WFF deriving (Ord)


instance Eq WFF where
    (Const x)                          == (Const y)                          = x == y
    Var s                              == Var s'                             = s == s'
    Not x                              == Not y                              = x == y
    And x y                            == And x' y'                          = x == x' && y == y'
    Or x y                             == Or x' y'                           = x == x' && y == y'
    Impl x y                           == Impl x' y'                         = x == x' && y == y'
    Equi x y                           == Equi x' y'                         = x == x' && y == y'

    -- True = False --> False
    (Const True)                       == (Const False) `Impl` (Const False) = True
    (Const False) `Impl` (Const False) == (Const True)                       = True

    -- \lnot x == x --> False
    Not x                              == (y `Impl` (Const False))           = x == y
    (y `Impl` (Const False))           == Not x                              = x == y

    -- x <--> y == (x --> y) /\ (y --> x)
    Equi x y                           == (x' `Impl` y') `And` (y'' `Impl` x'')
                                | and [x == x', x'' == x, y == y', y == y''] = True
                                | otherwise                                  = False

    Not (Not x)                        == y                                  = x == y
    y                                  == Not (Not x)                        = x == y
    x                                  == (y `Impl` (Const False)) `Impl` (Const False) = x == y
    (y `Impl` (Const False)) `Impl` (Const False) == x                              = x == y


    _                                  == _                                  = False

    x                                  /= y                                  = not (x == y)


instance Show WFF where
    show (Const x) = show x
    show (Var s) = s
    show (Not e) = ",-" ++ show e
    show (And x y) = "(" ++ show x ++ " /\\ " ++ show y ++ ")"
    show (Or x y)   = "(" ++ show x ++ " \\/ " ++ show y ++ ")"
    show (Impl x y) = "(" ++ show x ++ " --> " ++ show y ++ ")"
    show (Equi x y) = "(" ++ show x ++ " <--> " ++ show y ++ ")"


infix 1 <->
infix 2 -->
infix 3 \/
infix 4 /\

(∧) :: WFF -> WFF -> WFF
a ∧ b = And a b

(∨) :: WFF -> WFF -> WFF
a ∨ b = Or a b

(/\) :: WFF -> WFF -> WFF
a /\ b = And a b

(\/) :: WFF -> WFF -> WFF
a \/ b = Or a b

(-->) :: WFF -> WFF -> WFF
a --> b = Impl a b

(<->) :: WFF -> WFF -> WFF
a <-> b = Equi a b

(==>) :: Bool -> Bool -> Bool
True ==> False = False
_    ==> _     = True

(<==>) :: Bool -> Bool -> Bool
a <==> b
    | a == b = True
    | otherwise = False

eval :: Map.Map WFF Bool -> WFF -> Bool
eval m var@(Var _) = m Map.! var
eval _ (Const x)   = x
eval m (Not e)     = not (eval m e)
eval m (And x y)   = eval m x && eval m y
eval m (Or  x y)   = eval m x || eval m y
eval m (Impl x y)  = eval m x ==> eval m y
eval m (Equi x y)  = eval m x <==> eval m y

traverse :: (WFF -> a) -> WFF -> [a]
traverse f wwf@(Not e)   = f wwf : traverse f e
traverse f wwf@(And x y) = f wwf : (traverse f x ++ traverse f y)
traverse f wwf@(Or x y) = f wwf : (traverse f x ++ traverse f y)
traverse f wwf@(Impl x y) = f wwf : (traverse f x ++ traverse f y)
traverse f wwf@(Equi x y) = f wwf : (traverse f x ++ traverse f y)
traverse f wwf = [f wwf]

enumerate :: WFF -> [WFF]
enumerate = nub . traverse id