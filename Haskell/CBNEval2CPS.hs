{- Do CPS transformation, after CBNEval1ClosureConvCompl.
 -}
module CBNEval2CPS where
import DeBrujin

type Env = [DenVal]
data ExpVal = Funct Term Env
data DenVal = Thunk Term Env

eval :: Term -> Env -> (ExpVal -> omega) -> omega
eval (Var n)     e k =
  let Thunk t e' = (e !! n)
  in eval t e' k
eval (App t0 t1) e k =
  eval t0 e $ \(Funct t e') ->
    eval t (Thunk t1 e : e') k
eval (Abs t)     e k =
  k (Funct t e)

main :: Term -> ExpVal
main t = eval t [] id
