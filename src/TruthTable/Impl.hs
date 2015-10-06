module TruthTable.Impl ( buildTable
				  		, showTable
				  		, printTable ) where

import Data.List (intercalate, transpose)
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

