module Main ( main ) where

import Test.Framework (defaultMain, testGroup)
import qualified Test.Framework (Test)
import Test.Framework.Providers.HUnit

import Test.HUnit

import TruthTable.WWF
--a :: WWF
--a = And (Const True) (Var "P")

--c :: WWF
--c = (Var "P" ∧ Var "Q") --> (Var "P" ∨ Var "Q")

--d :: WWF
--d = (Var "P" ∨ Var "Q") --> (Var "Q" ∨ Var "P")

--e :: WWF
--e = ((Var "P" ∨ Var "Q") ∧ (Var "P" ∨ Var "R")) <--> (Var "P" ∧ (Var "Q" ∨ Var "R"))¨

test1, test2 :: Assertion
test1 = assertEqual "True is True" True True
test2 = assertEqual "False is not True" False True

tests :: [Test.Framework.Test]
tests =
    [ testGroup "default-group"
        [ testCase "test1" test1
        , testCase "test2" test2
        ]
    ]

main :: IO ()
main = defaultMain tests