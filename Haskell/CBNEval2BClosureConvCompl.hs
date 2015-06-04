{- Do closure conversion and inlining.
 -}
module CBNEval2ClosureConv where
import DeBrujin

type Env = [DenVal]
--newtype DenVal = Thunk { unThunk :: () -> ExpVal }
data ExpVal = Funct Term Env
data DenVal = Thunk Term Env

eval :: Term -> Env -> ExpVal
eval (Var n)     e =
  let Thunk t e2 = (e !! n)
  in eval t e2
eval (App t1 t2) e =
  let Funct t e2 = eval t1 e
  in
    eval t (Thunk t2 e : e2)
eval (Abs t)     e =
  Funct t e

main t = eval t []
