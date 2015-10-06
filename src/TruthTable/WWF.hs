module TruthTable.WWF (
      WWF(..), (∧), (∨), (-->), (<-->)
    , eval, traverse, enumerate
) where

import qualified  Data.Map as Map
import Data.List (nub)

data WWF = Const Bool
         | Var String
         | Not  WWF
         | And  WWF WWF
         | Or   WWF WWF
         | Impl WWF WWF
         | Equi WWF WWF deriving (Eq, Ord)

instance Show WWF where
    show (Const x) = show x
    show (Var s) = s
    show (Not e) = ",-" ++ show e
    show (And x y) = "(" ++ show x ++ " /\\ " ++ show y ++ ")"
    show (Or x y)   = "(" ++ show x ++ " \\/ " ++ show y ++ ")"
    show (Impl x y) = "(" ++ show x ++ " --> " ++ show y ++ ")"
    show (Equi x y) = "(" ++ show x ++ " <--> " ++ show y ++ ")"



(∧) :: WWF -> WWF -> WWF
a ∧ b = And a b


(∨) :: WWF -> WWF -> WWF
a ∨ b = Or a b

(-->) :: WWF -> WWF -> WWF
a --> b = Impl a b

(<-->) :: WWF -> WWF -> WWF
a <--> b = Equi a b

(==>) :: Bool -> Bool -> Bool
True ==> False = False
_    ==> _     = True

(<==>) :: Bool -> Bool -> Bool
a <==> b
    | a == b = True
    | otherwise = False

eval :: Map.Map WWF Bool -> WWF -> Bool
eval m var@(Var _) = m Map.! var
eval _ (Const x)   = x
eval m (Not e)     = not (eval m e)
eval m (And x y)   = eval m x && eval m y
eval m (Or  x y)   = eval m x || eval m y
eval m (Impl x y)  = eval m x ==> eval m y
eval m (Equi x y)  = eval m x <==> eval m y

traverse :: (WWF -> a) -> WWF -> [a]
traverse f wwf@(Not e)   = f wwf : traverse f e
traverse f wwf@(And x y) = f wwf : (traverse f x ++ traverse f y)
traverse f wwf@(Or x y) = f wwf : (traverse f x ++ traverse f y)
traverse f wwf@(Impl x y) = f wwf : (traverse f x ++ traverse f y)
traverse f wwf@(Equi x y) = f wwf : (traverse f x ++ traverse f y)
traverse f wwf = [f wwf]

enumerate :: WWF -> [WWF]
enumerate = nub . traverse id