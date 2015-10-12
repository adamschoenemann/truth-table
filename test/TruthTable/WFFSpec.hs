
module TruthTable.WFFSpec where

import SpecHelper

import Test.QuickCheck

import qualified Data.Map as Map

instance Arbitrary WFF where
    arbitrary = do
        b <- elements [True, False]
        return (Const b)

spec :: Spec
spec = do
    describe "WFF" $ do
        describe "And" $ do
            it "is commutative" $ property $
                \x y -> eval Map.empty (x `And` y) == eval Map.empty (y `And` x)
        describe "Or" $ do
            it "is commutative" $ property $
                \x y -> eval Map.empty (x `Or` y) == eval Map.empty (y `Or` x)

main :: IO ()
main = hspec spec
