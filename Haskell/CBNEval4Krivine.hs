{- Manual refactorings described in the paper to get Krivine machine, after CBNEval3Defunc. -}
module CBNEval4Krivine where
import DeBrujin

type Env = [Thunk]
data Thunk = Thunk Term Env
type Stack = [Thunk]

eval :: Term -> Env -> Stack -> Thunk
eval (Var n)     e k =
  let Thunk t e' = (e !! n)
  in eval t e' k
eval (App t0 t1) e k =
  eval t0 e $ (Thunk t1 e) : k
eval (Abs t)     e [] =
  Thunk t e
eval (Abs t)     e ((Thunk t1 e') : k) =
  eval t (Thunk t1 e' : e) k

main :: Term -> Thunk
main t = eval t [] []
