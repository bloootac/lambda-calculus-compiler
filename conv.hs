module Conv where

import Parser
import Lexer
import Data.Map (Map)
import qualified Data.Map as Map


data Lambda = Var String | F String Lambda | Ap Lambda Lambda 

instance Show Lambda where
  show (Var s) = s
  show (F s l) = "\\" ++ s ++ "." ++ show l
  show (Ap x y) = wrap (show x) ++ wrap (show y) where wrap s = if (length s == 1) then s else "(" ++ s ++ ")"


conv_to_lambda :: Map String Lambda_term -> [Line] -> [Lambda_term]
conv_to_lambda _ [] = [] 
conv_to_lambda dict (l:ls) = 
  case l of (Assign s f) -> conv_to_lambda (Map.insert s f dict) ls
            (Run x)      -> (conv_term x dict) : conv_to_lambda dict ls


conv_term :: Lambda_term -> Map String Lambda_term -> Lambda_term
conv_term l dict = 
  case l of (Apply x y)          -> Apply (conv_term x dict) (conv_term y dict)
            (Func ps x)          -> Func ps (conv_term x new_dict)
                                      where new_dict = foldr f dict ps 
                                            f p d      = Map.insert p (Variable (Str p)) d 
            (Variable (Str v))   -> case (Map.lookup v dict) of (Just i) -> if i == (Variable (Str v)) then i else conv_term i dict
                                                                Nothing  -> error "undefined variable"
            (Variable (Num n))   -> Variable (Num n) 



-- we aren't using numbers or funcs w/ multiple args anymore, so moving to this nicer data type (?)
undo_shorthand :: Lambda_term -> Lambda
undo_shorthand l = case l of (Variable (Str v)) -> Var v
                             (Variable (Num v)) -> F "f" (F "x" (num_to_lambda v))
                             (Func ps f)        -> foldr F (undo_shorthand f) ps
                             (Apply x y)        -> Ap (undo_shorthand x) (undo_shorthand y)

num_to_lambda :: Int -> Lambda
num_to_lambda 0 = Var "x"
num_to_lambda n = Ap (Var "f") (num_to_lambda $ n-1)

run = (map undo_shorthand) . (conv_to_lambda Map.empty) . parser . lexer

-- TODO: Lambda -> SK 
