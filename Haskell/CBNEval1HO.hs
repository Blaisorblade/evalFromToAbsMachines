module CBNEval1HO where
import DeBrujin

newtype DenVal = Thunk { unThunk :: () -> ExpVal }
newtype ExpVal = Funct { unFunct :: DenVal -> ExpVal }
type Env = [DenVal]
  
eval :: Term -> Env -> ExpVal
eval (Var n)     e =
  unThunk (e !! n) ()
eval (App t1 t2) e =
  let f = unFunct (eval t1 e)
  in
    f (Thunk (\() -> eval t2 e))
eval (Abs t)     e =
  Funct (\v -> eval t (v : e))

main :: Term -> ExpVal
main t = eval t []
