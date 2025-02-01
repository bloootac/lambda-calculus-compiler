module Comb where

import Control.Monad
import Data.List
import Data.Char

data Comb = S | K | V String | App Comb Comb deriving Eq

instance Show Comb where
  show (App (App S K) K) = "I"
  show S = "S"
  show K = "K"
  show (V s) = s
  show (App x y) = let sx = show x; sy = show y
                       wrap s = if (length s == 1) then s else "(" ++ s ++ ")"
				   in wrap sx ++ wrap sy
  -- show (App x y) =
    -- case is_num (App x y) of (Just n) -> show n 
                             -- Nothing  -> let sx = show x; sy = show y
                                             -- wrap s = if (length s == 1) then s else "(" ++ s ++ ")"
                                             -- is_num = (foldr (&&) True ). map isDigit
                                         -- in if (is_num sx && is_num sy) then "(" ++ sx ++ ")" ++ sy
                                            -- else wrap sx ++ wrap sy		
											

c_id = (App (App S K) K)

-- (===) :: Comb -> Comb -> Bool
-- K === App (App S (App (App S (App K S)) (App (App S (App K K)) K)) ) (App K c_id) = True
-- App (App S (App (App S (App K S)) (App (App S (App K K)) K)) ) (App K c_id) === K = True

-- S === ((App S ((App S (App K S)) `App` (App (App S (App K (App S (App K S)))) (App (App S (App K (App S (App K K)))) S)))) `App` (App K (App K c_id))) = True
-- ((App S ((App S (App K S)) `App` (App (App S (App K (App S (App K S)))) (App (App S (App K (App S (App K K)))) S)))) `App` (App K (App K c_id))) === S = True

-- (App S (App K K)) === ((App S (App (App S (App K S)) (App (App S (App K K)) (App (App S (App K S)) K)))) `App` (App K K)) = True
-- ((App S (App (App S (App K S)) (App (App S (App K K)) (App (App S (App K S)) K)))) `App` (App K K)) === (App S (App K K)) = True

-- (App (App S (App K S)) (App S (App K K))) === ((App S (App K K)) `App` ((App S (App (App S (App K S)) (App (App S (App K K)) (App (App S K) K)))) `App` (App K c_id))) = True
-- ((App S (App K K)) `App` ((App S (App (App S (App K S)) (App (App S (App K K)) (App (App S K) K)))) `App` (App K c_id))) === (App (App S (App K S)) (App S (App K K))) = True

-- (App S (App K (App S (App K S)))) `App` (App (App S (App K S)) (App S (App K S))) === ((App S (App (App S (App K S)) (App (App S (App K K)) (App (App S (App K S)) (App (App S (App K (App S (App K S)))) S))))) `App` (App K S)) = True
-- ((App S (App (App S (App K S)) (App (App S (App K K)) (App (App S (App K S)) (App (App S (App K (App S (App K S)))) S))))) `App` (App K S)) === ((App S (App K (App S (App K S)))) `App` (App (App S (App K S)) (App S (App K S)))) = True
-- K === K = True
-- S === S = True
-- (App x y) === (App x' y') = (x === x') && (y === y')
-- _ === _ = False


data Run a = Run a 

instance Show (Run Comb) where
  show (Run x) = show x
  
instance Applicative Run where
  pure = return
  (<*>) (Run f) (Run x) = Run (f x)
  
instance Functor Run where
  fmap f (Run x) = Run (f x)

instance Monad Run where
  return a      = Run a
  (Run a) >>= f = f a



data Annotated a = Log [String] a 

deriving instance Show a => Show (Annotated a)

instance Applicative Annotated where
  pure = return
  (<*>) = ap

instance Functor Annotated where
  fmap f (Log s a) = Log s (f a)
  
instance Monad Annotated where
  --return :: Show a => a -> Annotated a
  return x = Log [] x 
  (Log a x) >>= f = let (Log b y) = f x in Log (a ++ b) y
  
m_run_comb :: Monad m => (Comb -> m Comb) -> Comb -> m Comb
m_run_comb m (App (App K a) b) = m (App (App K a) b) >> m_run_comb m a >>= return 
m_run_comb m (App (App (App S f) g) x) = m (App (App (App S f) g) x) >> m_run_comb m (App (App f x) (App g x)) >>= return 
m_run_comb m (App x y) = do
               x' <- m_run_comb m x
               y' <- m_run_comb m y
               if (x == x' && y == y') then m (App x y) >>= return
                                       else m (App x y) >> m_run_comb m (App x' y')
m_run_comb m x = return x

run_comb :: Comb -> Run Comb
run_comb x = m_run_comb return x

annotated_run_comb :: Comb -> Annotated Comb
annotated_run_comb x = m_run_comb (\c -> Log [show c] c) x

show_annotated (Log s x) = mapM_ putStrLn (s ++ [show x])
log_run_comb = show_annotated . annotated_run_comb 

run_c :: Comb -> Comb

-- run_c (App (App S (App (App S (App K S)) (App (App S (App K K)) K)) ) (App K c_id)) = K
-- run_c ((App S ((App S (App K S)) `App` (App (App S (App K (App S (App K S)))) (App (App S (App K (App S (App K K)))) S)))) `App` (App K (App K c_id))) = S
-- run_c ((App S (App (App S (App K S)) (App (App S (App K K)) (App (App S (App K S)) K)))) `App` (App K K)) = (App S (App K K))
-- run_c ((App S (App K K)) `App` ((App S (App (App S (App K S)) (App (App S (App K K)) (App (App S K) K)))) `App` (App K c_id))) = (App (App S (App K S)) (App S (App K K)))
-- run_c ((App S (App (App S (App K S)) (App (App S (App K K)) (App (App S (App K S)) (App (App S (App K (App S (App K S)))) S))))) `App` (App K S)) = (App S (App K (App S (App K S)))) `App` (App (App S (App K S)) (App S (App K S)))

run_c (App (App K a) b) = run_c a
run_c (App (App (App S f) g) x) = run_c (App (App f x) (App g x))
run_c (App x y) = let x' = run_c x
                      y' = run_c y
                  in if (x == x' && y == y') then App x y else run_c (App x' y')
run_c x = x
   
c_length :: Comb -> Int
c_length S = 1
c_length K = 1
c_length (V x) = 1
c_length (App x y) = c_length x + c_length y
													   
c_zero = App (App S (App (App S (App K S)) (App K K))) (App K K)
c_plus_one = App S (App (App S (App K S)) (App (App S (App K K)) (App (App S K) K) ))

c_num :: Int -> Comb
c_num 0 = c_zero
c_num n = App c_plus_one (c_num $ n-1)

is_num :: Comb -> Maybe Int 
is_num c = is_num' c 0

is_num' :: Comb -> Int -> Maybe Int
is_num' c n = let x = c_num n
              in if c_length c < c_length x then Nothing
                 else if c == x then Just n
                      else is_num' c (n+1)

-- for testing
one = (S `App` ((S `App` (K `App` S)) `App` ((S `App` (K `App` K)) `App` c_id))) `App` (K `App` c_id)
fx f = (App (App f (V "f")) (V "x"))



{-
  recognise more forms of numbers:
   - numbers plus one? added to each other?
   - delayed id
   - numbers passed into other funcs e.g. fib, multiplying ?
-}
