module L.Interpreter where

import L.Types.AST
  import Data.Bits

type Env = Var -> Int

push :: Env -> Var -> Int -> Env
push env var value = \v ->
  if var == v then value else env v

emptyEnv :: Env
emptyEnv _ = error "Variable doesn't exist"

evalE :: Expression -> Env -> Int
evalE exp env = case exp of
  (Variable v) -> env v
  (Constant i) -> i
  (Operation op expl expr) ->
    let l = evalE expl env
        r = evalE expr env
        b2i b = if b then 1 else 0
    in case op of
      Plus -> l + r
      Minus -> l - r
      Multiply -> l * r
      Divide -> l / r
      And -> l .&. r
      Or -> l .|. r
      Xor -> l `xor` r
      Eq -> b2i $ l == r
      Lt -> b2i $ l < r
      Gt -> b2i $ l > r

type State = ([Int], [Int], Env)

evalS :: Statement -> State -> State
evalS statement (i, o, env)@state = case statement of
  Skip -> state
  (Seq s1 s2) ->
    let state'  = evalS s1 state
        state'' = evalS s2 state'
    in state''
  (Assign v exp) -> (i, o, push env v $ evalE exp env)
  (Read v) ->
    let (x:is) = i
    in (is, o, push env v x)
  (Write exp) -> (i, evalE exp env : o, env)
  (If exp s1 s2)
    | evalE exp env /= 0 -> evalS s1 state
    | otherwise -> evalS s2 state
  (While exp s)
    | evalE exp env == 0 -> state
    | otherwise ->
      let state' = evalS s state
      in evalS (While exp s) state'

evalP :: Program -> [Int] -> [Int]
evalP (Program s) input =
  let (i, o, env) = evalS s (input, [], emptyEnv)
  in reverse o
