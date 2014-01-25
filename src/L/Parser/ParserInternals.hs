module L.Parser.ParserInternals
       (
         makeOp
       ) where
import L.Types.AST
import qualified L.Types.Token as T

makeOp:: T.Token -> Op
makeOp t = let (T.Operator s) = t in
  case s of
    T.Plus -> Plus
    T.Minus -> Minus
    T.Multiply -> Multiply
    T.Divide -> Divide
    T.And -> And
    T.Or -> Or
    T.Xor -> Xor
    T.Eq -> Eq
    T.Lt -> Lt
    T.Gt -> Gt
