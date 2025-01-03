{
module Lexer where

import Prelude hiding (EQ)
}

%wrapper "basic"

$digit = 0-9                 -- digits
$alpha = [a-zA-Z]            -- alphabetic characters
$whitespace = [\ \t\f\v\r]   -- whitespace excluding \n

--TODO: rules to add later - combinators, memory + lookup, loops

tokens :-

  [ \n ]                           { \s -> NEWLINE      }
  true                             { \s -> TRUE         }
  false                            { \s -> FALSE        }
  run                              { \s -> RUN          }
  $alpha [$alpha $digit \_ \']*    { \s -> ID s         }
  \.                               { \s -> DOT          }
  \\                               { \s -> LAMBDA       }
  \=                               { \s -> EQ           }
  \(                               { \s -> LBRACE       }
  \)                               { \s -> RBRACE       }
  $digit+                          { \s -> INT (read s) }
  $whitespace+                     ;
  "--".*                           ;
{
-- Each action has type :: String -> Token

-- The token type:
data Token
  = ID String
  | DOT
  | LAMBDA
  | EQ
  | TRUE
  | FALSE
  | LBRACE
  | RBRACE
  | INT Int
  | NEWLINE
  | RUN
  deriving (Eq, Show)

lexer = alexScanTokens

main = do
  s <- getContents
  print (alexScanTokens s)
}