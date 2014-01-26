module L.Types.AST where

data Expression = Variable Var | Constant Int
                | Operation Op Expression Expression

data Var = Var String

data Op = Plus | Minus | Multiply | Divide
        | And | Or | Xor
        | Eq | Lt | Gt

data Statement = Skip | Seq Statement Statement
               | Assign Var Expression | Read Var | Write Expression
               | If Expression Statement Statement
               | While Expression Statement

data Program = Program Statement


instance Show Var where
  show (Var x) = x

instance Show Op where
  show o = case o of
    Plus -> "+"
    Minus -> "-"
    Multiply -> "*"
    Divide -> "/"
    And -> "&"
    Or -> "|"
    Xor -> "^"
    Eq -> "=="
    Lt -> "<"
    Gt -> ">"

instance Show Expression where
  show e = case e of
    (Variable v) -> show v
    (Constant i) -> show i
    (Operation o e1 e2) -> "(" ++ show e1 ++ ")" ++  show o
                           ++ "(" ++ show e2 ++ ")"

instance Show Statement where
  show s = case s of
    Skip -> "skip"
    (Seq s1 s2) -> show s1 ++ ";\n" ++ show s2
    (Assign v e) -> show v ++ " := " ++ show e
    (Read v) -> "read(" ++ show v ++ ")"
    (Write e) -> "write(" ++ show e ++ ")"
    (If e s1 s2) -> "if (" ++ show e ++ ") {\n"
                    ++ indent (show s1)
                    ++ "\n} else {\n"
                    ++ indent (show s2)
                    ++ "\n}"
    (While e s1) -> "while (" ++ show e ++ ") {\n"
                    ++ indent (show s1)
                    ++ "\n}"

    where
      indent = unlines . map ("  " ++) . lines

instance Show Program where
  show (Program s) = show s ++ "\n"
