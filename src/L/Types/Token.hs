module L.Types.Token
       (
         Token(..),
         Sym(..),
       ) where


data Token = Number String | Name String | Operator Sym | LParen | RParen
           | Skip | Semicolon | If | Else | While | Read | Write | Assign
           | LBrace | RBrace
           deriving Show

data Sym = Plus | Minus | Multiply | Divide
        | And | Or | Xor
        | Eq | Lt | Gt
        deriving Show
