{-# LANGUAGE TemplateHaskell #-}

module Subst(Subst(), domain, single, empty, apply, compose, restrictTo, allVars, testSubst) where
--module Subst(Subst(), domain, single, empty, apply, compose, restrictTo, allVars) where
import Type
import Pretty
import Vars
import Test.QuickCheck
import Data.List

-- Abstrakter Datentyp Subst mit Liste von Variablennamen und Funktion welche Variablennamen auf Terme abbildet
data Subst = Subst [VarName] (VarName -> Term)

-- Instanz von Show um eine Substitution auszugeben
instance Show Subst where
  show = pretty

-- Instanz von Pretty für Subst
{-
Nutzt die Funktionen der Substitution und zwei Hilfsfunktionen um diese formatiert auszugeben.
h nutzt apply um die Substitution mit einem gegebenen Variablennamen auszuführen
  Der Output von h ist pretty variablenname -> pretty term auf den die Variable abgebildet wird
h' fügt einen gegebenen String (hier ", ") zwischen jedes Element einer Liste von Strings und konkatiniert diese
-}
instance Pretty Subst where
  pretty s = "{" ++ h' ", " (map h (domain s)) ++ "}" -- h wird auf jeden Variablennamen angewand, den die domain der Substitution zurückliefert
   where
    h :: VarName -> String
    h vname = pretty vname ++ " -> " ++ pretty (apply s (Var vname))
    h' :: String -> [String] -> String
    h' _ []       = []
    h' _ [x]      = x
    h' a (x : xs) = x ++ a ++ h' a xs

-- Instanz von Vars für Subst
instance Vars Subst where
  allVars (Subst xs func) =
    nub (xs ++ foldr (\x y -> allVars (func x) ++ y) [] xs)

-- Instanz von Arbitrary für Subst
instance Arbitrary Subst where
  arbitrary = do
    vts <- arbitrary                                 -- vts :: [(VarName, Term)]
    let xs' = nub (map fst vts)
    let func x = case lookup x vts of                -- schaut ob ein Variablennamen in einem Tupel aus vts vorkommt und definiert die Abbildung entsprechend
          Nothing -> Var x
          Just t  -> t
    let xs'' = filter (\x -> Var x /= func x) xs'    -- Filtert alle Variablen, die auf sich selbst abgebildet werden
    return (Subst xs'' func)

-- Gibt die domain einer Substitution zurück
-- hier keine Dupplikateliminierung, weil die Eindeutigkeit der Substitution an jeder anderen Stelle gewährt wird
domain :: Subst -> [VarName]
domain (Subst vs _) = vs

-- Die leere Substitution
empty :: Subst
empty = Subst [] Var

-- Eine einstellige Substitution
-- wenn v == t gilt wird eine Variable auf sich selbst abgebildet, was der leeren Substitution entspricht.
single :: VarName -> Term -> Subst
single v t | Var v /= t = Subst [v] (\v' -> if v' == v then t else Var v')
           | otherwise  = empty

-- Wendet eine Substitution auf einen Term an
apply :: Subst -> Term -> Term
apply (Subst _ f) (Var x) = f x                         -- Anwenden der Funktion der Substitution auf den Term(=einfache Variable) 
apply s (Comb n xs)       = Comb n (map (apply s) xs)   -- Wendet die Substitution auf jeden Term in der vorkommenden Liste an

-- Eine Mehrstellige Substitution bzw. Komposition zweier Substitutionen
compose :: Subst -> Subst -> Subst
compose s@(Subst xs _) (Subst ys g) =  let f' = apply s . g                                          -- Anwenden der ersten Substitution auf das Ergebnis der zweiten Substitution
                                       in  Subst (filter (\x -> Var x /= f' x) (nub  (ys ++ xs))) f' -- Duplikateliminierung über nub [VarName] und filtern der Variablennamen, auf die f' abbildet 

-- Schränkt eine Subsitution bzw. ihren Definitionsbereich auf eine gegebene Menge von Variablen ein
-- Filtert die Variablennamen der Substitution über die gegebene Menge
restrictTo :: Subst -> [VarName] -> Subst
restrictTo (Subst xs func) vnames = Subst (filter (`elem` vnames) xs) (\x' -> if x' `elem` vnames then func x' else Var x')


-- Hilfsfunktion welche überprüft, ob ys eine Teilmenge von xs ist
-- xs ⊆ ys
subset :: Eq a => [a] -> [a] -> Bool
subset xs ys = all (`elem` ys) xs
-- 1
prop_apply_empty :: Term -> Bool
prop_apply_empty t = apply empty t == t
-- 2
prop_apply_single :: VarName -> Term -> Bool
prop_apply_single vname t = apply (single vname t) (Var vname) == t
-- 3
prop_apply_comp :: Subst -> Subst -> Term -> Bool
prop_apply_comp s1 s2 t = apply (compose s1 s2) t == apply s1 (apply s2 t)
-- 4
prop_empty_domain :: Bool
prop_empty_domain = null (domain empty)
-- 5
prop_id_domain :: VarName -> Bool
prop_id_domain vname = null (domain (single vname (Var vname)))
-- 6
prop_domain :: Term -> VarName -> Property
prop_domain t vname = t /= Var vname ==> domain (single vname t) == [vname]
-- 7
prop_domain_subset :: Subst -> Subst -> Bool
prop_domain_subset s1 s2 = subset (domain (compose s1 s2)) (domain s1 ++ domain s2)
-- 8
prop_domain_comp :: VarName -> VarName -> Property
prop_domain_comp x1 x2 = x1 /= x2
                         ==> domain (compose (single x2 (Var x1)) (single x1 (Var x2))) 
                         == [x2]
-- 9
prop_allVars_empty :: Bool
prop_allVars_empty = null (allVars empty)
-- 10
prop_allVars_single_id :: VarName -> Bool
prop_allVars_single_id vname = null (allVars (single vname (Var vname)))
-- 11 & 12
prop_allVars_single :: VarName -> Term -> Bool
prop_allVars_single vname t | Var vname == t = null (allVars (single vname (Var vname)))
                            | otherwise = let all_vars = allVars (single vname t)
                                          in  all_vars `subset` (vname : allVars t) && (vname : allVars t) `subset` all_vars
-- 13 
prop_allVars_compose_subset :: Subst -> Subst -> Bool
prop_allVars_compose_subset s1 s2 = allVars (compose s1 s2) `subset` (allVars s1 ++ allVars s2)
-- 15 (die Gleichheit wie auf mathematischen Mengen ist schwirgig, deswegen zwei mal subset testen)
prop_allVars_compose :: VarName -> VarName -> Property
prop_allVars_compose x1 x2 = x1 /= x2 ==> toCheck `subset` [x1, x2] && [x1, x2] `subset` toCheck
                                    where toCheck = allVars (compose (single x2 (Var x1)) (single x1 (Var x2)))
-- 14
prop_domain_subset_allVars :: Subst -> Bool
prop_domain_subset_allVars s = domain s `subset` allVars s
-- 15
prop_restrict_empty :: [VarName] -> Bool
prop_restrict_empty xs = null (domain (restrictTo empty xs))
-- 16
prop_restrict :: [VarName] -> Subst -> Bool
prop_restrict xs s = domain (restrictTo s xs) `subset` xs

-- Um alle Properties in diesem Modul zu testen (wegen Bug mit IDE auskommentiert)
--return []
--testSubst :: IO Bool
--testSubst = $quickCheckAll

-- Hard-Code der obigen auskommentierten Funktion
testSubst :: IO Bool
testSubst = do
  putStrLn "prop_apply_empty"
  r1 <- quickCheckResult prop_apply_empty
  putStrLn "prop_apply_single"
  r2 <- quickCheckResult prop_apply_single
  putStrLn "prop_apply_comp"
  r3 <- quickCheckResult prop_apply_comp
  putStrLn "prop_empty_domain"
  r4 <- quickCheckResult prop_empty_domain
  putStrLn "prop_id_domain"
  r5 <- quickCheckResult prop_id_domain
  putStrLn "prop_domain"
  r6 <- quickCheckResult prop_domain
  putStrLn "prop_domain_subset"
  r7 <- quickCheckResult prop_domain_subset
  putStrLn "prop_domain_comp"
  r8 <- quickCheckResult prop_domain_comp
  putStrLn "prop_allVars_empty"
  r9 <- quickCheckResult prop_allVars_empty
  putStrLn "prop_allVars_single_id"
  r10 <- quickCheckResult prop_allVars_single_id
  putStrLn "prop_allVars_single"
  r11 <- quickCheckResult prop_allVars_single
  putStrLn "prop_allVars_compose_subset"
  r12 <- quickCheckResult prop_allVars_compose_subset
  putStrLn "prop_allVars_compose"
  r13 <- quickCheckResult prop_allVars_compose
  putStrLn "prop_domain_subset_allVars"
  r14 <- quickCheckResult prop_domain_subset_allVars
  putStrLn "prop_restrict_empty"
  r15 <- quickCheckResult prop_restrict_empty
  putStrLn "prop_restrict"
  r16 <- quickCheckResult prop_restrict
  return (all isSuccess [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16])



