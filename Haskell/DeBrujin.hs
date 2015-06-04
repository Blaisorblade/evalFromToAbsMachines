module DeBrujin where

data Term = Var Int
          | Abs Term
          | App Term Term
