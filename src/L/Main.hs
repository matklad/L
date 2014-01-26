module Main where
import System.Environment
import System.IO
import L.Parser.Parser
import L.Interpreter.Interpreter

main:: IO()
main = do
  (f:_) <- getArgs
  source <- readFile f
  let program = parse source
  textInput <- getContents
  let input = map read $ words textInput
  let output = evalP program input
  let textOutput = unwords $ map show output
  putStrLn textOutput
