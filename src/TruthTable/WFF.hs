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
         | Equi WFF WFF deriving (Eq, Ord)

instance Show WFF where
    show (Const x) = show x
    show (Var s) = s
    show (Not e) = ",-" ++ show e
    show (And x y) = "(" ++ show x ++ " /\\ " ++ show y ++ ")"
    show (Or x y)   = "(" ++ show x ++ " \\/ " ++ show y ++ ")"
    show (Impl x y) = "(" ++ show x ++ " --> " ++ show y ++ ")"
    show (Equi x y) = "(" ++ show x ++ " <--> " ++ show y ++ ")"



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