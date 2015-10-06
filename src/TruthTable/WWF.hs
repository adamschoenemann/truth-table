module TruthTable.WWF (WWF(..), (∧), (∨), (-->), (<-->)) where

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