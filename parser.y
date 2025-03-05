{
module Parser where

import Lexer 
import Prelude hiding (EQ)
}

%name parser
%tokentype { Token }
%error { parseError }
%token
      ID              { ID $$    }
      DOT             { DOT      }
      LAMBDA          { LAMBDA   }
      '='             { EQ       }
      TRUE            { TRUE     }
      FALSE           { FALSE    }
      '('             { LBRACE   }
      ')'             { RBRACE   }
      INT             { INT $$   }
      NEWLINE         { NEWLINE  }
      RUN             { RUN      }
	  
%left ID INT LAMBDA '(' ')' NEWLINE 
%nonassoc APPLY
%%

	  
program     : line                                       { [$1]         }
            | inner_line program                         { $1 : $2      }
	  
line        : ID '=' lambda_term                         { Assign $1 $3 }
            | RUN lambda_term                            { RunTerm $2   }
            | line NEWLINE                               { $1           }
	  
inner_line  : line NEWLINE                               { $1           }
	  
lambda_term : var                                        { Variable $1  }
            | LAMBDA args DOT lambda_term                { Func $2 $4   }
            | lambda_term lambda_term %prec APPLY        { Apply $1 $2  }
            | '(' lambda_term ')'                        { $2           }

args        : ID                                         { [$1]         }
            | ID args                                    { $1 : $2      }

var         : ID                                         { Str $1       }
            | INT                                        { Num $1       }

var_list    : var                                        { [$1]         }
            | var var_list                               { $1 : $2      }
	  
	  
{

parseError :: [Token] -> a
parseError _ = error "Parse error"

data Line 
      = Assign String Lambda_term 
      | RunTerm Lambda_term
      deriving Show

data Lambda_term 
      = Func [String] Lambda_term 
      | Apply Lambda_term Lambda_term 
      | Variable Var
      deriving (Show, Eq)

data Var 
      = Str String
      | Num Int	 
	  deriving (Show, Eq)

	  
main = getContents >>= print . parser . lexer
 
}

