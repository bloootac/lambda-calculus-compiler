module Comb where

import Control.Monad
import Data.List
import Data.Char

-- ******************** datatypes ********************

data Comb = S | K | V String | App Comb Comb deriving Eq

instance Show Comb where
  show (App (App S K) K) = "I"
  show S = "S"
  show K = "K"
  show (V s) = s
  -- show (App x y) = let sx = show x; sy = show y
                       -- wrap s = if (length s == 1) then s else "(" ++ s ++ ")"
				   -- in wrap sx ++ wrap sy
  show (App x y) =
    case is_num (App x y) of (Just n) -> show n 
                             Nothing  -> let sx = show x; sy = show y
                                             wrap s = if (length s == 1) then s else "(" ++ s ++ ")"
                                             is_num = (foldr (&&) True ). map isDigit
                                         in if (is_num sx && is_num sy) then "(" ++ sx ++ ")" ++ sy
                                            else wrap sx ++ wrap sy		

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


-- ******************** monad run ********************

m_run_comb :: Monad m => (Comb -> m Comb) -> Comb -> m Comb
m_run_comb m (App (App K a) b) = m (App (App K a) b) >> m_run_comb m a >>= return 
m_run_comb m (App (App (App K a) b) c) = m (App (App (App K a) b) c) >> m_run_comb m (App a c) >>= return
m_run_comb m (App c (App (App K a) b)) = m (App c (App (App K a) b)) >> m_run_comb m (App c a) >>= return
m_run_comb m (App (App (App S K) a) b) = m (App (App (App S K) a) b) >> m_run_comb m b >>= return
m_run_comb m (App (App (App S f) g) x) = m (App (App (App S f) g) x) >> m_run_comb m (App (App f x) (App g x)) >>= return 

-- m_run_comb m (App x y) = do
               -- x' <- m_run_comb m x
               -- y' <- m_run_comb m y
               -- if (x == x' && y == y') then m (App x y) >>= return
                                       -- else m (App x y) >> m_run_comb m (App x' y')
m_run_comb m (App x y) = do				
                  x' <- m_simplify_one_step m x
                  if (x == x') then 
                      do y' <- m_simplify_one_step m y 
                         if (y == y') then m (App x y) >>= return else m (App x y') >> m_run_comb m (App x y')
                  else m (App x' y) >> m_run_comb m (App x' y)
				  
m_run_comb m x = return x

m_simplify_one_step :: Monad m => (Comb -> m Comb) -> Comb -> m Comb
m_simplify_one_step m (App (App K a) b) = m a >>= return 
m_simplify_one_step m (App (App (App S f) g) x) = m (App (App f x) (App g x)) >>= return 
m_simplify_one_step m (App x y) = do x' <- m_simplify_one_step m x
                                     if (x == x') then 
                                         do y' <- m_simplify_one_step m y 
                                            if (y == y') then m (App x y) >>= return else m (App x y') >>= return
                                     else m (App x' y) >>= return
m_simplify_one_step m x = return x

run_comb :: Comb -> Run Comb
run_comb x = m_run_comb return x


-- ******************** non-monad run ********************
-- (not used anymore, but i'll leave them here anyway)

run_c :: Comb -> Comb

run_c (App (App K a) b) = run_c a
run_c (App (App (App K a) b) c) = run_c (App a c)
run_c (App c (App (App K a) b)) = run_c (App c a)
run_c (App (App (App S K) a) b) = run_c b
run_c (App (App (App S f) g) x) = run_c (App (App f x) (App g x))
-- run_c (App x y) = let x' = run_c x
                      -- y' = run_c y
                  -- in if (x == x' && y == y') then App x y else run_c (App x' y')

run_c (App x y) = let x' = simplify_one_step x
                  in if (x == x') then 
                      let y' = simplify_one_step y 
                      in if (y == y') then (App x y) else run_c (App x y')
                  else run_c (App x' y)
                     
run_c x = x
   
simplify_one_step :: Comb -> Comb
simplify_one_step (App (App K a) b) = a
simplify_one_step (App (App (App S f) g) x) = (App (App f x) (App g x))
simplify_one_step (App x y) = let x' = simplify_one_step x
                              in if (x == x') then 
                                  let y' = simplify_one_step y 
                                  in if (y == y') then (App x y) else (App x y')
                              else (App x' y)
simplify_one_step x = x


-- ******************** logging ********************

annotated_run_comb :: Comb -> Annotated Comb
annotated_run_comb x = m_run_comb (\c -> Log [show c] c) x

-- i should make the logs better...
logged_comb :: Comb -> String
logged_comb c = let space = "            " in
                  case c of  (App (App (App S f) g) x) -> "complete: S" ++ space ++ show f ++ space ++ show g ++ space ++ show x ++ "\n"
                             (App (App K a) b)         -> "complete: K" ++space ++ show a ++ space ++ show b ++ "\n"
                             (App a b)                 -> "partial: " ++ show a ++ space ++ show b ++ "\n"
                             _                         -> show c

write_f_log func log c = do 
                         contents <- readFile log
                         let output = contents ++ "\n" ++ func c
                         when (length output > 0) $ writeFile log output
                         return c


write_run_comb :: Comb -> IO Comb
write_run_comb x = m_run_comb (write_f_log logged_comb "log.txt") x

show_annotated (Log s x) = mapM_ putStrLn (s ++ [show x])

-- log from terminal
log_run_comb = show_annotated . annotated_run_comb 

-- ******************** number detection ********************
   
c_length :: Comb -> Int
c_length S = 1
c_length K = 1
c_length (V x) = 1
c_length (App x y) = c_length x + c_length y

-- useful for testing
fx f = (App (App f (V "f")) (V "x"))


c_id = (App (App S K) K)
c_zero = App K c_id
c_plus_one = (App S (App (App S (App K S)) (App (App S (App K K)) c_id)))

c_num :: Int -> Comb
c_num 0 = c_zero
c_num n = App c_plus_one (c_num $ n-1)

is_num :: Comb -> Maybe Int 

-- plus one: S (S (KS) (S (KK) I)) i = i + 1
is_num (App (App S (App (App S (App K S)) (App (App S (App K K)) (App (App S K) K)))) i) = fmap (1+) (is_num i)

-- delayed const: S (S (KS) (S (S (KS) (S (KK) Ki) (S (KK) I)) )) 0 = i
is_num (App (App S (App (App S (App K S)) (App (App S (App (App S (App K S)) (App (App S (App K K)) (App K i)))) (App (App S (App K K)) (App (App S K) K))))) c_zero) = is_num i

-- another delayed const: ((S(Ki))I) = i
is_num (App (App S (App K i)) (App (App S K) K)) = is_num i

-- multiplication: (S(Ki))j = i * j
is_num (App (App S (App K i)) j) = is_num i >>= \x -> is_num j >>= \y -> return (x * y)

-- zero
is_num (App K (App (App S K) K)) = Just 0

-- not numbers
is_num x = Nothing

-- ******************** reduction tree search ********************

allSimplifications :: Comb -> [Comb]
allSimplifications K = []
allSimplifications S = []
allSimplifications (App (App K x) y) = x : (map (\x' -> App (App K x') y) $ allSimplifications x) ++ (map (\y' -> App (App K x) y') $ allSimplifications y)
allSimplifications (App (App (App S f) g) x) = (App (App f x) (App g x)) : (map (\f' -> App (App (App S f') g) x) $ allSimplifications f) ++ (map (\g' -> App (App (App S f) g') x) $ allSimplifications g) ++ (map (\x' -> App (App (App S f) g) x') $ allSimplifications x)
allSimplifications (App x y) = (map (\x' -> App x' y) $ allSimplifications x) ++ (map (\y' -> App x y') $ allSimplifications y)


shortestComputation :: Comb -> Int
shortestComputation K = 1
shortestComputation S = 1
shortestComputation x = let y = (map shortestComputation (allSimplifications x)) in if (y == []) then 0 else minimum y + 1

all_comp_lengths :: Comb -> [Int]
all_comp_lengths K = [0]
all_comp_lengths S = [0]
											 
all_comp_lengths x = case (allSimplifications x) of [] -> [0] ;
                                                    xs -> running_min id (+1) (concat $ map all_comp_lengths (xs))

all_comp_lengths' K = [(0, K)]
all_comp_lengths' S = [(0, S)]
all_comp_lengths' x = case (allSimplifications x) of [] -> [(0, x)]
                                                     xs -> running_min fst (\(a, b) -> (a + 1, b)) (concat $ map all_comp_lengths' xs)


log_comp_search :: Comb -> IO () 
log_comp_search x = let y = all_comp_lengths x
                        f = write_f_log (\x -> "found a path of length " ++ show x) "log.txt" 
                    in mapM_ f y 

	 
running_min :: (Ord b) => (a -> b) -> (a -> c) -> [a] -> [c]
running_min f g (x:xs) = g x : fil (f x) xs
  where
    fil _ [] = []
    fil min (y:ys)
      | f y > min = fil min ys
      | otherwise = g y : fil (f y) ys