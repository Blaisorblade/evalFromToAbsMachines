{-# LANGUAGE NoMonomorphismRestriction #-}

{-
Implement Felleisen and Friedman's CEK machine, following both Danvy and
Abstracting Abstract Machines.

Their presentations are slightly different, but they are related; maybe their
relation can be described through known refactorings. For instance, one can
inline eval and step (as presented here) and then disentangle the result to
create eval_value and continue.
-}

module CBVEvalCEK where

import qualified Data.Map as M
import           Data.Maybe (fromJust)

import           NamedLambda

type Env a = M.Map Name a

empty :: Env a
empty = M.empty

extend :: Name -> a -> Env a -> Env a
extend = M.insert

-- For simplicity, assume terms are well-scoped; this is easy to ensure, so this
-- assumption is even realistic.
lookup :: Name -> Env a -> a
lookup = (fromJust .) . M.lookup

data ExpVal = Closure Name Term (Env ExpVal)
data EvalContext = Stop
                 | Arg Term (Env ExpVal) EvalContext
                 | Fun ExpVal EvalContext

type MachineState = (Term, Env ExpVal, EvalContext)
step :: MachineState -> MachineState
step (Value (Var x), env, k) = (v, env', k)
  where
    c@(Closure name v env') = CBVEvalCEK.lookup x env
step (Value (Abs x body), env, Arg t env' k) = (t, env', Fun (Closure x body env) k)
step (Value (Abs x body), env, Fun (Closure y t env') k) = (t, extend y (Closure x body env) env', k)
--step (Value (Abs x body), env, Stop) is missing on purpose!
step (Comp (App t0 t1), env, k) = (t0, env, Arg t1 env k)

eval :: MachineState -> ExpVal
eval (Value (Abs x body), env, Stop) = Closure x body env
eval state = eval (step state)

evalInit = eval . initState

initState t = (t, empty, Stop)
