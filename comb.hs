module Comb where

import Control.Monad

data Comb = S | K | V String | App Comb Comb deriving Eq

instance Show Comb where
  show (App (App S K) K) = "I"
  show S = "S"
  show K = "K"
  show (V s) = s
  show (App x y) = wrap (show x) ++ wrap (show y) where wrap s = if (length s == 1) then s else "(" ++ s ++ ")"

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