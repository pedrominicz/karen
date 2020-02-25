{
module Lex
  ( Token(..)
  , tokenize
  ) where
}

%wrapper "basic"

$alpha = [A-Za-z]
$digit = [0-9]
$upper = [A-Z]

tokens :-
  $white+                       ;
  \%.*                          ;
  \,                            { \s -> Comma }
  \.                            { \s -> Dot }
  :\-                           { \s -> Turnstile }
  \(                            { \s -> OpenParen }
  \)                            { \s -> CloseParen }
  _                             { \s -> Variable "_" }
  $upper [$alpha $digit _]*     { \s -> Variable s }
  $alpha [$alpha $digit _]*     { \s -> Atom s }

{
data Token
  = Comma
  | Dot
  | Turnstile
  | OpenParen
  | CloseParen
  | Variable String
  | Atom String
  deriving (Show)

tokenize :: String -> Maybe [Token]
tokenize str = go ('\n', [], str)
  where
  go input@(_, _, str) =
    case alexScan input 0 of
      AlexEOF            -> Just []
      AlexError _        -> Nothing
      AlexSkip input len -> go input
      AlexToken input len act -> do
        rest <- go input
        return $ act (take len str) : rest
}
