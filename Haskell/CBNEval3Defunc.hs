{- Do defunctionalization, after CBNEval2CPS. -}
module CBNEval3Defunc where
import DeBrujin

type Env = [DenVal]
data ExpVal = Funct Term Env
data DenVal = Thunk Term Env
data Cont = InitCont
          | Cont Term Env Cont


applyCont InitCont t = t
applyCont (Cont t1 e k) (Funct t e') = eval t (Thunk t1 e : e') k

eval :: Term -> Env -> Cont -> ExpVal
eval (Var n)     e k =
  let Thunk t e' = (e !! n)
  in eval t e' k
eval (App t0 t1) e k =
  eval t0 e $ Cont t1 e k
eval (Abs t)     e k =
  applyCont k (Funct t e)

main :: Term -> ExpVal
main t = eval t [] InitCont
