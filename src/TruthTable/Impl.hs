module TruthTable.Impl ( buildTable
				  		, showTable
				  		, printTable ) where

import Data.List (nub, intercalate, transpose)
import Data.Maybe (fromJust)
import qualified Data.Map as Map

import TruthTable.WWF

type TruthTable = [Map.Map WWF Bool]
alignTable :: [[String]] -> [[String]]
alignTable = transpose . (map align) . transpose
    where
        align xs =
            let maxlen = maximum     (map length xs)
            in  map (pad maxlen) xs where
                pad to x = x ++ replicate (to - (length x)) ' '

showTable :: TruthTable -> [[String]]
showTable [] = [[]]
showTable (r:rs) =
    let header = map show $ Map.keys r
        rows   = map (map show . Map.elems) (r:rs)
    in  [header] ++ rows

printTable :: TruthTable -> IO ()
printTable t  = do
    mapM_ (putStrLn . intercalate " | ") $ alignTable $ showTable t


buildTable :: WWF -> TruthTable
buildTable wwf = let m = permuteMap $ vars wwf
                 in map (\m' -> expand m' wwf) $ m

(==>) :: Bool -> Bool -> Bool
True ==> False = False
_    ==> _     = True

(<==>) :: Bool -> Bool -> Bool
a <==> b
    | a == b = True
    | otherwise = False


sample :: [[a]] -> [[a]]
sample []   = []
sample [xs] = map (\x -> [x]) xs
sample [xs, ys] = [[x,y] | x <- xs, y <- ys]
sample (xs:xss) = concat $ map (\x -> map (x:) (sample xss)) xs

expand :: Map.Map WWF Bool -> WWF -> Map.Map WWF Bool
expand m w = Map.fromList $ map (\w' -> (w', eval m w')) $ enumerate w

permuteMap :: (Ord k) => Map.Map k [v] -> [Map.Map k v]
permuteMap m =
    let list = Map.toList m
        vals = map snd list
        keys = map fst list
        indices = sample $ vals
    in  map (Map.fromList . zip keys) indices

vars :: WWF -> Map.Map WWF [Bool]
vars = Map.fromList . map (\w -> (w, [True, False])) . (filter isVar) . enumerate
    where
        isVar (Var _) = True
        isVar _       = False

eval :: Map.Map WWF Bool -> WWF -> Bool
eval m var@(Var v) = m Map.! var
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
