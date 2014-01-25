module Main where
import L.Parser.Lexer
import L.Parser.Parser

main:: IO()
main = do
  s <- getContents
  let tokens = tokenize s
  let p = parse tokens
  print p
