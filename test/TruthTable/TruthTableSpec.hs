module TruthTable.TruthTableSpec where

import SpecHelper

import qualified Data.Map as Map

p, q :: WFF
p = Var "P"
q = Var "Q"


spec :: Spec
spec = do
    describe "TruthTable" $ do
        describe "buildTable" $ do
            it "works with AND" $ do
                let andTable =
                        [ Map.fromList [(p, True),  (q, True),  (p `And` q, True) ]
                        , Map.fromList [(p, True),  (q, False), (p `And` q, False)]
                        , Map.fromList [(p, False), (q, True),  (p `And` q, False)]
                        , Map.fromList [(p, False), (q, False), (p `And` q, False)]
                        ]
                buildTable (p ∧ q) `shouldBe` andTable

            it "works with OR" $ do
                let orTable =
                        [ Map.fromList [(p, True),  (q, True),  (p `Or` q, True) ]
                        , Map.fromList [(p, True),  (q, False), (p `Or` q, True)]
                        , Map.fromList [(p, False), (q, True),  (p `Or` q, True)]
                        , Map.fromList [(p, False), (q, False), (p `Or` q, False)]
                        ]
                buildTable (p ∨ q) `shouldBe` orTable
            it "works with Impl" $ do
                let implTable =
                        [ Map.fromList [(p, True),  (q, True),  (p --> q, True) ]
                        , Map.fromList [(p, True),  (q, False), (p --> q, False)]
                        , Map.fromList [(p, False), (q, True),  (p --> q, True)]
                        , Map.fromList [(p, False), (q, False), (p --> q, True)]
                        ]
                buildTable (p --> q) `shouldBe` implTable

        context "Should work" $ do
            it "asserts correctly" $ do
                True `shouldBe` True


main :: IO ()
main = hspec spec
