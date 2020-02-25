{
module Parse
  ( parse
  , parseFile
  ) where

import Lex
import Syntax
}

%tokentype { Token }

%monad { Maybe } { (>>=) } { return }
%error { const Nothing }

%name parse term_list
%name parseFile clause_list

%token
  ','   { Comma }
  '.'   { Dot }
  ':-'  { Turnstile }
  '('   { OpenParen }
  ')'   { CloseParen }
  var   { Variable $$ }
  atom  { Atom $$ }
%%

term_list :: { [Term] }
  : term term_list_             { $1 : $2 }

term_list_ :: { [Term] }
  : ',' term term_list_         { $2 : $3 }
  | '.'                         { [] }

term :: { Term }
  : var                         { Var $1 }
  | atom args                   { Term $1 $2 }

args :: { [Term] }
  : '(' term args_              { $2 : $3 }
  |                             { [] }

args_ :: { [Term] }
  : ',' term args_              { $2 : $3 }
  | ')'                         { [] }

clause_list :: { [Clause] }
  : clause clause_list          { $1 : $2 }
  |                             { [] }

clause :: { Clause }
  : term ':-' term_list         { Clause $1 $3 }
  | term '.'                    { Clause $1 [] }
