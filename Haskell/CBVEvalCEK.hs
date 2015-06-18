{-# LANGUAGE NoMonomorphismRestriction #-}

{-
Implement Felleisen and Friedman's CEK machine, following both Danvy on the one
hand, and Van Horn & Might, Abstracting Abstract Machines, on the other hand.

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
step (Value (Var x), env, k) = (Value (Abs name v), env', k)
  where
    c@(Closure name v env') = CBVEvalCEK.lookup x env

step (Value (Abs x body), env, Arg t env' k) = (t, env', Fun (Closure x body env) k)
step (Value (Abs x body), env, Fun (Closure y t env') k) = (t, extend y (Closure x body env) env', k)
--step (Value (Abs x body), env, Stop) is missing on purpose!
step (Comp (App t0 t1), env, k) = (t0, env, Arg t1 env k)

-- This matches eval' in Van Horn and Might, Sec. 2.1, page 52.
eval :: MachineState -> ExpVal
eval (Value (Abs x body), env, Stop) = Closure x body env
eval state = eval (step state)

evalInit = eval . initState

initState t = (t, empty, Stop)

-- By manual but mechanical inlining of eval and step, we get the following
-- definition of the CEK machine:
eval2 :: MachineState -> ExpVal
eval2 (Value (Var x), env, k) = eval2 (Value (Abs name v), env', k)
  where
    c@(Closure name v env') = CBVEvalCEK.lookup x env
eval2 (Value (Abs x body), env, Arg t env' k) = eval2 (t, env', Fun (Closure x body env) k)
eval2 (Value (Abs x body), env, Fun (Closure y t env') k) = eval2 (t, extend y (Closure x body env) env', k)
eval2 (Value (Abs x body), env, Stop) = Closure x body env
eval2 (Comp (App t0 t1), env, k) = eval2 (t0, env, Arg t1 env k)

-- By disentangling the nested match on k inside Value (Abs ...), we extract continue3:
eval3 :: MachineState -> ExpVal
eval3 (Value (Var x), env, k) = eval3 (Value (Abs name v), env', k)
  where
    c@(Closure name v env') = CBVEvalCEK.lookup x env
eval3 (Value (Abs x body), env, k) = continue (Closure x body env) k
eval3 (Comp (App t0 t1), env, k) = eval3 (t0, env, Arg t1 env k)

continue clos (Arg t env' k) = eval3 (t, env', Fun clos k)
continue clos (Fun (Closure y t env') k) = eval3 (t, extend y clos env', k)
continue clos Stop = clos

-- By disentangling the nested match inside Value, we extract evalValue:

eval4 :: MachineState -> ExpVal
eval4 (Value v, env, k) = evalValue v env k
eval4 (Comp (App t0 t1), env, k) = eval4 (t0, env, Arg t1 env k)

evalValue (Var x) env k = eval4 (Value (Abs name v), env', k)
  where
    c@(Closure name v env') = CBVEvalCEK.lookup x env
evalValue (Abs x body) env k = continue4 (Closure x body env) k

continue4 clos (Arg t env' k) = eval4 (t, env', Fun clos k)
continue4 clos (Fun (Closure y t env') k) = eval4 (t, extend y clos env', k)
continue4 clos Stop = clos

-- Inlining the call to eval4 (Value (Abs ...), and inlining the resulting call to evalValue (Abs ...), we get:

eval5 :: MachineState -> ExpVal
eval5 (Value v, env, k) = evalValue5 v env k
eval5 (Comp (App t0 t1), env, k) = eval5 (t0, env, Arg t1 env k)

-- After only one inlining step:
-- evalValue (Var x) env k = evalValue (Abs name v) env' k
-- After one more inlining step:
-- evalValue5 (Var x) env k = continue5 (Closure name v env') k
-- Since we're now rebuilding the same closure we matched on:
{-
evalValue5 (Var x) env k = continue5 c k
  where
    c = CBVEvalCEK.lookup x env
-}
-- Inlining c:
evalValue5 (Var x) env k = continue5 (CBVEvalCEK.lookup x env) k
evalValue5 (Abs x body) env k = continue5 (Closure x body env) k

continue5 clos (Arg t env' k) = eval5 (t, env', Fun clos k)
continue5 clos (Fun (Closure y t env') k) = eval5 (t, extend y clos env', k)
continue5 clos Stop = clos

-- Finally, by factoring the call to continue5 in evalValue5, we get Danvy's
-- variant of the CEK machine:
eval6 :: MachineState -> ExpVal
eval6 (Value v, env, k) = continue6 (evalValue6 v env) k
eval6 (Comp (App t0 t1), env, k) = eval6 (t0, env, Arg t1 env k)

evalValue6 (Var x) env = CBVEvalCEK.lookup x env
evalValue6 (Abs x body) env = Closure x body env

continue6 clos (Arg t env' k) = eval6 (t, env', Fun clos k)
continue6 clos (Fun (Closure y t env') k) = eval6 (t, extend y clos env', k)
continue6 clos Stop = clos

eval6Init = eval6 . initState
