module LambdaCalculusNaive where
 
import Control.Monad
import Data.Either
import Data.List
import Data.Maybe
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Prim
 
-- | Определяем типы
type Name = String
 
data Term
  = Var Name
  | App Term
        Term
  | Lam Name
        Term
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
[dot, lambda, lambda', brLeft, brRight] = ['.', '\\', '\\', '(', ')']
 
-- | Красиво показываем термы (вариант 1)
ppTerm1 :: Term -> String
ppTerm1 (Var s) = s
ppTerm1 (Lam x e) = concat [[brLeft], [lambda'], x, [dot], ppTerm1 e, [brRight]]
ppTerm1 (App e1 e2) = concat [[brLeft], ppTerm1 e1, ppTerm1 e2, [brRight]]
 
-- | Красиво показываем термы (вариант 2) 
ppTerm2 :: Term -> String
ppTerm2 (Var s) = s
ppTerm2 (Lam x e) = concat [[brLeft], [lambda], x, arrow, ppTerm2 e, [brRight]]
ppTerm2 (App e1 e2) = concat [[brLeft], ppTerm2 e1, ppTerm2 e2, [brRight]]
 
-- | Красиво показываем термы (вариант 3) (меньше скобок) 
ppTerm3 :: Term -> String
ppTerm3 (Var s) = s
ppTerm3 (Lam x e) = concat [[lambda'], x, arrow, ppTerm3 e]
ppTerm3 (App e1 e2) = concat [pp e1, delimiter, pp e2]
  where
    pp term@(Var s) = ppTerm3 term
    pp term@(Lam x e) = parens $ ppTerm3 term
    pp term@(App e1 e2) = parens $ ppTerm3 term
    parens term = concat [[brLeft], term, [brRight]]
 
-- | Парсер комбинаторы 
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
 
-- | Парсер
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
 
--  Альфа конверсия, все связанные переменные получают новые имена
renames :: [String] -> Term -> Term
renames source (Lam x e) = Lam newName $ renames (tail updateSource) e2
  where
    newName = head updateSource
    updateSource = source \\ freeVars e
    e2 = subst x (Var newName) e
renames source (App e1 e2) = App (renames source e1) (renames source e2)
renames _ x = x
 
-- | Бетта редукция
--   subst v e1 e2 подставляет терм e1 вместо переменной v в терме e2 
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
 
-- | Наивный вычислитель (медленный из-за альфа-конверсий)
--   операционная семантика, вызов по имени, нормальная стратегия
eval :: Term -> Term
eval (Var s) = Var s
eval (App e1 e2) =
  case eval e1 of
    Lam x e -> eval (subst x e2 e)
    _ -> App e1 (eval e2)
eval (Lam x e) = Lam x (eval e)
 
-- | Канонические имена для тестирования (используем то, что переменные
--   односимвольные и именуем начиная с 'a')
evalCan = (renames allNames) . (renames allNames') . eval
--Пробовать например так --- вычисление 2 в 3 степени
--evalCan (parseTerm "((\\a.(\\b.(a(a(ab)))))(\\c.(\\d.(c(cd)))))")
-- (пробелы в терме не допускаются).