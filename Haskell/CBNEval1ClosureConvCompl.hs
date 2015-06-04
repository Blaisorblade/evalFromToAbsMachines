{- Do closure conversion and inlining.
 -}
module CBNEval1ClosureConvCompl where
import DeBrujin

type Env = [DenVal]
data ExpVal = Funct Term Env
data DenVal = Thunk Term Env

eval :: Term -> Env -> ExpVal
eval (Var n)     e =
  let Thunk t e' = (e !! n)
  in eval t e'
eval (App t0 t1) e =
  let Funct t e' = eval t0 e
  in
    eval t (Thunk t1 e : e')
eval (Abs t)     e =
  Funct t e

main :: Term -> ExpVal
main t = eval t []
