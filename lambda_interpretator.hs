-- {-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Lab where
import Prelude
import Data.List ((\\), union)
import Text.ParserCombinators.Parsec
    ( char, letter, many1, (<|>), getInput, parse, Parser )
import Text.ParserCombinators.Parsec.Prim ()
import Control.Monad.List ( replicateM )
import System.IO ( stdout, hFlush )


---------------------------------------------------------------------------------------
--i :: a -> a
--i = \x -> x
--k :: p1 -> p2 -> p1
--k = \x y -> x
--s :: (t1 -> t2 -> t3) -> (t1 -> t2) -> t1 -> t3
--s = \f g x -> f x (g x)


--class Expr where
--    i :: a -> a
--    k :: b1 -> b2 -> b1
--    s :: (t1 -> t2 -> t3) -> (t1 -> t2) -> t1 -> t3 

--instance Expr => Show a where
--  show a -> a = "i"
--instance Expr String where
--    i = \x -> x
--    k = \x -> (\y -> x )
--    s = \f g x -> f x (g x)
---------------------------------------------------------------------------------------



--проверяет количество скобок и правильность записи выражения                          
check_correct :: [Char] -> String 
helper1 :: Int -> Int -> [Char] -> String 
check_correct list_str = helper1 0 0 list_str
helper1 lb rb list_str    | (list_str /= []) && not (check_symbol (head list_str)) = "the expression is Incorrect"
                          | (list_str /= []) && ((head list_str) == '(')  = helper1 (lb + 1) rb (tail list_str)
                          | (list_str /= []) && ((head list_str) == ')') = helper1 lb (rb + 1) (tail list_str)
                          | (list_str /= []) = helper1 lb rb (tail list_str)
                          | (lb == rb) = "the expression is Correct"
                          | otherwise = "the expression is Incorrect"
--Проверка символов 
check_symbol :: Char -> Bool 
check_symbol symb | ((symb == 'I') || (symb == 'K') || (symb == 'S') || (symb == '(') || (symb == ')')) = True
                  | otherwise = False


--Удалить элемент из списка по индексу
deleteAt :: Int -> [a] -> [a]
deleteAt idx xs = lft ++ rgt
  where (lft, (_:rgt)) = splitAt idx xs


--убираем левоассоциативные скобки 
delete_left_associative_brackets :: [Char] -> String 
helper_del :: Int -> String ->String -> String

delete_left_associative_brackets lst_str = helper_del 1 lst_str lst_str 
helper_del count_br lst_str lst_str_source  | (count_br == 1) && ((length lst_str) /= (length lst_str_source)) = if ((head p) /= '(') 
                                                                                                                then p
                                                                                                                else delete_left_associative_brackets p   
                                            | ((head lst_str) == '(') = helper_del (count_br + 1) (tail lst_str) lst_str_source
                                            | (head lst_str) == ')' =  helper_del (count_br - 1) (tail lst_str) lst_str_source
                                            | (count_br == 1) && ((length lst_str) == (length lst_str_source)) = lst_str_source
                                            | otherwise = helper_del count_br (tail lst_str) lst_str_source
                                              where p = (reverse (deleteAt (length lst_str )  (reverse (drop 1 lst_str_source))))

--перевод из char в строку                                           
toString :: Char -> String 
toString ch = [ch]


--Токенизатор (возвращает лист токенов)
tokenize :: String -> [String]
helper :: String -> Int -> String -> [String] -> [String] 
tokenize _str_ = helper _str_ 1 "" []   
helper str_  count_br temp_str tokens   | (count_br == 1) && (temp_str /="")=  helper str_ count_br "" ( tokens ++ [temp_str]) 
                                        | (str_ == "") = tokens                                                  
                                        | ((head str_) == '(') = helper (tail str_) (count_br + 1) (temp_str ++ [(head str_)]) tokens
                                        | (head str_) == ')' =  helper (tail str_) (count_br - 1) (temp_str ++ [(head str_)]) tokens
                                        | otherwise = helper (tail str_) count_br (temp_str ++ [(head str_)]) tokens



--Перевод из листа в строку
from_lst_to_string :: [String] -> String
helper_l_to_s :: [String] -> String -> String
from_lst_to_string lst = helper_l_to_s lst ""       
helper_l_to_s lst_ str' | (lst_ == []) = str'
                        | otherwise = helper_l_to_s (tail lst_) (str' ++ (head lst_))                                            


type Name = String
 
data Term = Var Name
          | App Term Term
          | Lam Name Term
          | Sub Term Name Term
            deriving (Eq, Ord, Show)
 
-- | Вспомогательные выражения
wrap :: a -> [a]
wrap = replicate 1
arrow :: [Char]
arrow = " -> "
delimiter :: [Char]
delimiter = " "
brRight :: Char
brLeft :: Char
lambda' :: Char
lambda :: Char
dot :: Char
[dot, lambda, lambda', brLeft, brRight] = ['.', '!', '/', '(', ')']


-- Парсер комбинаторы 
term :: Parser Term
term = lam <|> app <|> var <|> bracket
 
var :: Parser Term
var = Var . wrap <$> letter
 
lam :: Parser Term
lam = Lam <$> (char lambda *> (wrap <$> letter) <* char dot) <*> term
 
bracket :: Parser Term
bracket = char brLeft *> term <* char brRight
 
app :: Parser Term
app = foldl1 App <$> many1 (var <|> bracket)
 
chunk :: Parser String
chunk = term >> getInput

--  Парсер
parseTerm :: String -> Term
parseTerm str =
  case parse chunk "" str of
    Left msg -> error $ show msg
    Right str' ->
      if not (null str')
        then error $ show "Left some input"
        else case parse term "" str of
               Left msg' -> error $ show msg'
               Right term -> term


ppTerm1 :: Term -> String
ppTerm1 (Var s) = s
ppTerm1 (Lam x e) = concat [[brLeft], [lambda], x, [dot], ppTerm1 e, [brRight]]
ppTerm1 (App e1 e2) = concat [[brLeft], ppTerm1 e1, ppTerm1 e2, [brRight]]


convert_from_combinators_to_lambda_expr :: String -> String
helper_convert :: String -> String -> String
convert_from_combinators_to_lambda_expr str = helper_convert str ""
helper_convert str'' str_new  | (str'' == []) = str_new
                              | (head str'' == 'S') = helper_convert (tail str'')  (str_new ++ "(/a.(/b.(/c.(ac(bc)))))") 
                              | (head str'' == 'K') = helper_convert (tail str'')  (str_new ++ "(/d.(/e.d))")
                              | (head str'' == 'I') = helper_convert (tail str'')  (str_new ++ "(/f.f)") 
                              | (head str'' == '(') = helper_convert (tail str'')  (str_new ++ "(") 
                              | (head str'' == ')') = helper_convert (tail str'')  (str_new ++ ")")
                              | otherwise = "Couldnt convert expression to lambda term"


-- | Ищем все свободные переменные в терме
freeVars :: Term -> [Name]
freeVars (Var s) = [s]
freeVars (App e1 e2) = freeVars e1 `union` freeVars e2
freeVars (Lam x e) = freeVars e \\ [x]
 
-- Бесконечный список имен для альфа конверсий
allNames :: [String]
allNames = [1 ..] >>= (`replicateM` ['a' .. 'z'])
 
allNames' :: [String]
allNames' = [2 ..] >>= (`replicateM` ['a' .. 'z'])
 
subst :: Name -> Term -> Term -> Term
subst v t f@(Var s)
  | s == v = t
  | otherwise = f
subst v t (App e1 e2) = App (subst v t e1) (subst v t e2)
subst v t f@(Lam x e)
  | x == v = f
  | otherwise =
    if x `notElem` freeVars t
      then Lam x (subst v t e)
      else let f' = renames allNames f -- альфа конверсия всех связанных перменных в f
            in subst v t f'
 



--  Альфа конверсия, все связанные переменные получают новые имена
renames :: [String] -> Term -> Term
renames source (Lam x e) = Lam newName $ renames (tail updateSource) e2
  where
    newName = head updateSource
    updateSource = source \\ freeVars e
    e2 = subst x (Var newName) e
renames source (App e1 e2) = App (renames source e1) (renames source e2)
renames _ x = x



app1 :: Term -> Term
app1 (App (Lam var body) env) = (Sub body var env)
app1 a = a

abst :: Term -> Term
abst (Lam var (App body (Var var')))
  | var == var' = body
abst a = a

sub :: Term -> Term
sub (Sub f@(Var var') var env)
  | var == var' = env
  | otherwise = f
sub (Sub (App a b) var env) = (App  (Sub a var env) (Sub b var env))
sub (Sub f@(Lam var' body) var env)
  |  var == var' = (Lam var body)
  | otherwise = if var `notElem` freeVars body 
                        then (Lam var' (Sub body var env))
                         else let f' = renames allNames f -- альфа конверсия всех связанных перменных в f
                              in (Sub body var f')
sub a = a

eval :: Term -> Term
eval expr@(Var _) = expr
eval expr@(Lam a b) = abst (Lam a (eval b))
eval expr@(App a b) = app1 (App (eval a) (eval b))
eval expr@(Sub a b c) = sub (Sub (eval a) b (eval c))

evalWhile :: Term -> Term
evalWhile a
  | a == b = a
  | otherwise = evalWhile b
  where b = eval a

evalCan :: Term -> Term
evalCan = (renames allNames) . (renames allNames') . evalWhile

main :: IO ()
main = do
    putStrLn "Choose your preferred option:"
    putStrLn "1 - Combinators"
    putStrLn "2 - Lambda expression"
    putStrLn "P.S."
    putStrLn "   Spaces are not avalible (also at the end of the expression)"
    putStrLn "   It uses the fact that variables are single-character and their names better starting from 'a' to 'z' "
    putStrLn "   Example for lambda expr: /x.(x)"
    putStrLn "   Example for combinators expr: S(KS)"
    putStrLn "Waiting for your input..."
    hFlush stdout
    ns <- getLine
    case () of _ 
                | (ns == "1") -> do {putStrLn "Enter an expression:";  
                                    hFlush stdout; 
                                    nk <- getLine; 
                                    (putStrLn (check_correct nk));
                                    putStrLn(delete_left_associative_brackets (ppTerm1 (evalCan (parseTerm (convert_from_combinators_to_lambda_expr nk)))))
                                     }
                | (ns == "2") -> do {putStrLn "Enter an expression:";
                                     hFlush stdout; 
                                     nf <- getLine; 
                                     putStrLn(delete_left_associative_brackets (ppTerm1 (evalCan (parseTerm nf))))
                                    }
                | (ns /= "1") && (ns /= "2") -> do {putStrLn "Invalid input"}


