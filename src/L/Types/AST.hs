module L.Types.AST where

data Expression = Variable Var | Constant Int
                | Operation Op Expression Expression
                deriving Show

data Var = Var String
         deriving Show

data Op = Plus | Minus | Multiply | Divide
        | And | Or | Xor
        | Eq | Lt | Gt
        deriving Show

data Statement = Skip | Seq Statement Statement
               | Assign Var Expression | Read Var | Write Expression
               | If Expression Statement Statement
               | While Expression Statement
               deriving Show

data Program = Program Statement
             deriving Show
