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

prune_lim = 10

m_run_comb :: Monad m => (Comb -> m Comb) -> Comb -> m Comb
m_run_comb m (App (App K a) b) = m (App (App K a) b) >> m_run_comb m (k_prune prune_lim a) >>= return 
m_run_comb m (App (App (App K a) b) c) = m (App (App (App K a) b) c) >> m_run_comb m (k_prune prune_lim (App a c)) >>= return
m_run_comb m (App c (App (App K a) b)) = m (App c (App (App K a) b)) >> m_run_comb m (k_prune prune_lim (App c a)) >>= return
m_run_comb m (App (App (App S f) g) x) = m (App (App (App S f) g) x) >> m_run_comb m (k_prune prune_lim (App (App f x) (App g x))) >>= return 

m_run_comb m (App x y) = do				
                  x' <- m_simplify_one_step m x
                  if (x == x') then 
                      do y' <- m_simplify_one_step m y 
                         if (y == y') then m (App x y) >>= return else m (App x y') >> m_run_comb m (k_prune prune_lim (App x y'))
                  else m (App x' y) >> m_run_comb m (k_prune prune_lim (App x' y))
				  
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

run_c :: Comb -> Comb

run_c (App (App K a) b) = run_c $ k_prune prune_lim a
run_c (App (App (App K a) b) c) = run_c $ k_prune prune_lim (App a c)
run_c (App c (App (App K a) b)) = run_c $ k_prune prune_lim(App c a)
run_c (App (App (App S f) g) x) = run_c $ k_prune prune_lim (App (App f x) (App g x))

run_c (App x y) = let x' = simplify_one_step x
                  in if (x == x') then 
                      let y' = simplify_one_step y 
                      in if (y == y') then k_prune prune_lim (App x y) else run_c $ k_prune prune_lim (App x y')
                  else run_c $ k_prune prune_lim (App x' y)
                     
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

-- prune all K instances in the top n layers
k_prune :: Int -> Comb -> Comb

k_prune 0 x                 = x
k_prune 1 (App (App K x) y) = x
k_prune 1 x                 = x

k_prune n (App (App K x) y) = k_prune (n-2) x --if the code doesn't work, this (n-2) might be negative sometimes
k_prune n (App x y) = App (k_prune (n-1) x) (k_prune (n-1) y)

k_prune _ x = x

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

-- ******************** string -> comb ********************

-- i think this works ! 
s_comb :: String -> Comb
s_comb = snd . stringToComb

stringToComb :: String -> (String, Comb)

stringToComb (x:[]) = ([], char_to_comb x)

stringToComb ('(' : xs) = let (s,c)   = stringToComb xs in
                            if (s == "") then (s, c)
                            else if (head s == ')') then (tail s, c)
                                 else let (s',c') = stringToComb s  in
                                          (s',c `App` c')
							  

stringToComb (')':xs) = stringToComb xs

stringToComb (x : y : xs) = if y == ')' then (xs, char_to_comb x)
                            else if y /= '(' then (tail xs, App (char_to_comb x) (char_to_comb y))
                                 else let (s, c) = stringToComb (y: xs) in (s, App (char_to_comb x) c )


char_to_comb :: Char -> Comb
char_to_comb x 
  | x == 'K' = K
  | x == 'S' = S
  | x == 'I' = App (App S K) K
  | isDigit x = c_num $ read [x]
  | otherwise = V [x]  			   

-- ******************** number detection ********************
   
c_length :: Comb -> Int
c_length S = 1
c_length K = 1
c_length (V x) = 1
c_length (App x y) = c_length x + c_length y

-- useful for testing!
fx f = (App (App f (V "f")) (V "x"))
mfx f = fx $ App f (V "m")

c_id = (App (App S K) K)
c_zero = App K c_id
c_plus_one = (App S (App (App S (App K S)) K))

c_num :: Int -> Comb
c_num 0 = c_zero
c_num n = App c_plus_one (c_num $ n-1)

is_num :: Comb -> Maybe Int 


is_num n = let m = match_num n
           in if m == Nothing then is_num_fx $ run_c $ fx n else m

match_num x = 
--                   (S((S(KS))K))i = i + 1
  case optimise x of (App (App S (App (App S (App K S)) K)) i) -> fmap (1+) (is_num i)
  
--                   (S((S(KS))((S(K(S(Ki))))K)))0 = i
                     (App (App S (App (App S (App K S)) (App (App S (App K (App S (App K i)))) K))) (App K (App (App S K) K))) -> is_num i

--                   (S(Ki))j = i * j
                     (App (App S (App K i)) j) -> (is_num i >>= \x -> is_num j >>= \y -> return (x * y)) ;

--                   (S(1(Ki)))I = i
					 (App (App S (App (App S (App K (App (App S K) K))) (App K i))) (App (App S K) K)) -> is_num i

--                   KI = 0
                     (App K (App (App S K) K)) -> Just 0 ;

--                   anything else 
					 x                         -> Nothing


-- TODO: getting 1 = I = S (KI) bc of optimisations ... is this okay ? 

is_num_fx :: Comb -> Maybe Int
is_num_fx n = case n of (App (V "f") c) -> fmap (1+) $ is_num_fx c
                        (V "x")         -> Just 0 ;
                         _              -> Nothing
 
-- ******************** optimisations ********************
-- (should maybe be in conv.hs instead ... but match_num needs this) 

optimise :: Comb -> Comb

-- S (Ka) (Kb) -> K (ab)
optimise (App (App S (App K a)) (App K b)) = App K (optimise (App a b))
-- S (Ka) I -> a
optimise (App (App S (App K a)) (App (App S K) K)) = optimise a
-- S (KK) a b -> K ab
optimise (App (App (App S (App K K)) a) b) = App K (optimise (App a b)) 

-- S I (Ka) b -> ba 
optimise (App (App (App S (App (App S K) K)) (App K a)) b) =  optimise (App b a) 

-- S KI -> I
optimise (App S (App K (App (App S K) K))) =  c_id

-- S (K (S KK)) K -> S KK K
optimise (App (App S (App K (App S (App K K)))) K) = App (App S (App K K)) K

optimise (App x y) = let x' = optimise x
                         y' = optimise y
                     in if (x==x' && y==y') then (App x y) else optimise (App x' y')
optimise x = x


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