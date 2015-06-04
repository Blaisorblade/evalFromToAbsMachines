{- Just do closure conversion without inlining.
 -}
module CBNEval2ClosureConv where
import DeBrujin

type Env = [DenVal]
--newtype DenVal = Thunk { unThunk :: () -> ExpVal }
data ExpVal = Funct Term Env
data DenVal = Thunk Term Env

unFunct :: ExpVal -> DenVal -> ExpVal
unFunct (Funct t e) v = eval t (v : e)

unThunk :: DenVal -> ExpVal
unThunk (Thunk t e) = eval t e

eval :: Term -> Env -> ExpVal
eval (Var n)     e =
  unThunk (e !! n)
eval (App t1 t2) e =
  let f = unFunct (eval t1 e)
  in
    f (Thunk t2 e)
eval (Abs t)     e =
  Funct t e

main t = eval t []
