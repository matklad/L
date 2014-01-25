module L.Parser.LexerInternals
       (
         readSymToken
       ) where
import L.Types.Token

readSymToken:: String -> Token
readSymToken s = Operator $ case s of
  "+" -> Plus
  "-" -> Minus
  "*" -> Multiply
  "/" -> Divide
  "&" -> And
  "|" -> Or
  "^" -> Xor
  "==" -> Eq
  "<" -> Lt
  ">" -> Gt
