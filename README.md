truth-table
===========

A small Haskell library which can build truth-tables from boolean expressions

Example
-------

```haskell
p, q, r :: WWF
p = Var "P"
q = Var "Q"
r = Var "R"

printTable $ buildTable (p `And` q) `And` (p `Or` r))
-- Outputs
P     | Q     | R     | (P /\ Q) | ((P /\ Q) /\ (P \/ R)) | (P \/ R)
True  | True  | True  | True     | True                   | True
True  | True  | False | True     | True                   | True
True  | False | True  | False    | False                  | True
True  | False | False | False    | False                  | True
False | True  | True  | False    | False                  | True
False | True  | False | False    | False                  | False
False | False | True  | False    | False                  | True
False | False | False | False    | False                  | False
```