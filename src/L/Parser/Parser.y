{
module L.Parser.Parser (parse) where

import qualified L.Types.Token as T
import L.Types.AST
import L.Parser.ParserInternals

}

%name parse
%tokentype { T.Token }
%error { parseError }

%token
    number    { T.Number $$ }
    var       { T.Name $$ }
    '('       { T.LParen }
    ')'       { T.RParen }
    skip      { T.Skip }
    ';'       { T.Semicolon }
    if        { T.If }
    else      { T.Else }
    while     { T.While }
    read      { T.Read }
    write     { T.Write }
    assign    { T.Assign }
    '{'       { T.LBrace }
    '}'       { T.RBrace }
    '+'       { T.Operator T.Plus }
    '-'       { T.Operator T.Minus }
    '*'       { T.Operator T.Multiply }
    '/'       { T.Operator T.Divide }
    '&'       { T.Operator T.And }
    '|'       { T.Operator T.Or }
    '^'       { T.Operator T.Xor }
    eq        { T.Operator T.Eq }
    '<'       { T.Operator T.Lt }
    '>'       { T.Operator T.Gt }


%left ';'
%left '>' '<' eq
%left '&' '|' '^'
%left '+' '-'
%left '*' '/'

%%

Program ::  { Program }
  : Statement                   { Program $1 }

Statement :: { Statement }
  : skip                        { Skip }
  | Statement ';' Statement     { Seq $1 $3 }
  | var assign Expression       { Assign (Var $1) $3 }
  | read '(' var ')'            { Read (Var $3) }
  | write '(' Expression ')'    { Write $3 }
  | if '(' Expression ')' '{' Statement '}' else '{' Statement '}'
                                { If $3 $6 $10 }
  | while '(' Expression ')' '{' Statement '}'
                                { While $3 $6 }

Expression :: { Expression }
  : var                         { Variable (Var $1) }
  | number                      { Constant (read $1) }
  | Expression '+' Expression   { Operation (makeOp $2) $1 $3}
  | Expression '-' Expression   { Operation (makeOp $2) $1 $3}
  | Expression '*' Expression   { Operation (makeOp $2) $1 $3}
  | Expression '/' Expression   { Operation (makeOp $2) $1 $3}
  | Expression '&' Expression   { Operation (makeOp $2) $1 $3}
  | Expression '|' Expression   { Operation (makeOp $2) $1 $3}
  | Expression '^' Expression   { Operation (makeOp $2) $1 $3}
  | Expression '<' Expression   { Operation (makeOp $2) $1 $3}
  | Expression '>' Expression   { Operation (makeOp $2) $1 $3}
  | Expression eq  Expression   { Operation (makeOp $2) $1 $3}

{

parseError :: [T.Token] -> a
parseError t = error "You shall not parse"

}
