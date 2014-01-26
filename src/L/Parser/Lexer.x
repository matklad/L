{

module L.Parser.Lexer (tokenize) where

import L.Parser.LexerInternals
import L.Types.Token

}

%wrapper "basic"

$digit   = 0-9
$alpha   = [a-zA-Z]

tokens :-
  $white+               ;
  "//".*                ;

  "skip"                        { \s -> Skip}
  "if"                          { \s -> If}
  "else"                        { \s -> Else}
  "while"                       { \s -> While}
  "read"                        { \s -> Read}
  "write"                       { \s -> Write}

  $digit+                       { Number }
  $alpha ($alpha | $digit)*     { Name }
  [\+\-\*\/\&\|\^\<\>]          { readSymToken }
  "=="                          { readSymToken }
  ":="                          { \s -> Assign}

  "("                           { \s -> LParen }
  ")"                           { \s -> RParen }
  "{"                           { \s -> LBrace }
  "}"                           { \s -> RBrace }
  ";"                           { \s -> Semicolon}

{

tokenize :: String -> [Token]
tokenize = alexScanTokens

}
