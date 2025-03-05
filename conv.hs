module Conv where

import Parser
import Comb
import Lexer
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad
import System.IO

{-
TODO: 
more optimisations:
 - add S rule
 - add combinators
 - maybe look into subOne later

binary encoding: S = 01, K = 00, App = 1
 - write result to file
 - write stack func in C

-}

-- ******************** Program -> Lambda ********************

data Lambda = Var String | F String Lambda | Ap Lambda Lambda deriving Eq

instance Show Lambda where
  show (Var s) = s
  show (F s l) = "\\" ++ s ++ "." ++ show l
  show (Ap x y) = wrap (show x) ++ wrap (show y) where wrap s = if (length s == 1) then s else "(" ++ s ++ ")"

conv_to_lambda :: Map String Lambda_term -> [Line] -> [Lambda_term]
conv_to_lambda _ [] = [] 
conv_to_lambda dict (l:ls) = 
  case l of (Assign s f) -> conv_to_lambda (Map.insert s f dict) ls
            (RunTerm x)  -> (conv_term x dict) : conv_to_lambda dict ls


conv_term :: Lambda_term -> Map String Lambda_term -> Lambda_term
conv_term l dict = 
  case l of (Apply x y)          -> Apply (conv_term x dict) (conv_term y dict)
            (Func ps x)          -> Func ps (conv_term x new_dict)
                                      where new_dict = foldr f dict ps 
                                            f p d      = Map.insert p (Variable (Str p)) d 
            (Variable (Str v))   -> case (Map.lookup v dict) of (Just i) -> if i == (Variable (Str v)) then i else conv_term i dict
                                                                Nothing  -> (Variable (Str v))
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


-- ******************** Lambda -> SK ********************

-- rules:
-- \\x.C = KC
-- \\x.x = SKK
-- \\x.AB = S (\\x.A) (\\x.B)

lambda_to_comb :: Lambda -> Comb

lambda_to_comb l =
  case l of (F x y)  -> lambda_to_comb' x (lambda_to_comb y)
            (Ap x y) -> App (lambda_to_comb x) (lambda_to_comb y) 
            (Var x)  -> V x

lambda_to_comb' :: String -> Comb -> Comb 
lambda_to_comb' x y =
  case y of (App a b) -> App (App S (lambda_to_comb' x a)) (lambda_to_comb' x b)
            otherwise -> if y == (V x) then App (App S K) K else App K y
			

-- ******************** run from terminal ********************

run_lambda = (map undo_shorthand) . (conv_to_lambda Map.empty) . parser . lexer

conv = (map (optimise . lambda_to_comb . undo_shorthand)) . (conv_to_lambda Map.empty) . parser . lexer
conv_single x = (conv x) !! 0

run = map run_comb . conv
run_single = run_comb . conv_single
run_fx = run_comb . fx . conv_single
show_run = putStr . unlines . map (show . run_comb) . conv


-- ******************** run from file ********************

run_file :: (String -> IO ()) -> IO ()
run_file r = do
  program <- readFile "program.txt"
  r program

f_conv = run_file (putStr . unlines . map show . conv)
run_f_comb = run_file (putStr . unlines . map (show . run_comb) . conv)
run_f_lambda = run_file (putStr . unlines . map show . run_lambda)
run_f_fx = run_file (putStr . unlines . map (show . run_comb . fx) . conv)

run_f_log = do
  program <- readFile "program.txt"
  (mapM_ write_run_comb . conv) program 

-- search reduction tree for normal form, log lengths of paths found
run_f_comp_paths ::  IO ()
run_f_comp_paths = run_file (log_comp_search . conv_single)

-- search reduction tree, print first normal form found
run_f_find_path :: IO()
run_f_find_path = run_file (putStrLn . show . run_comb . snd . (\x -> x !! 0) . all_comp_lengths' . conv_single)

