module NamedLambda where

{-
data Term = Var String
          | Abs Term
          | App Term Term
-}

-- Follow Sec. 2.2
data Term = Value Value
          | Comp Comp

type Name = String
data Value = Var Name
           | Abs Name Term
data Comp = App Term Term
