{-# LANGUAGE TemplateHaskell #-}

module Unify(ds, unify, testUnify) where
--module Unify(ds, unify) where
import Type
import Subst
import Vars()
import Data.Maybe
import Test.QuickCheck


-- Berechnet die Unstimmigkeitsmenge zweier Terme
ds :: Term -> Term -> Maybe (Term,Term)
-- Beide Terme sind Variablen
ds t1@(Var x) t2@(Var y)           | x == y                               = Nothing      -- gleiche Terme => ds = {}
                                   | x == VarName "_" || y == VarName "_" = Nothing      -- einer (oder beide) Terme sind annonyme Variablen => ds = {}
                                   | otherwise                            = Just (t1,t2) -- => ds = {t1 , t2}
-- Einer der Terme ist eine Variable
ds t1@(Var x) t2                   | x == VarName "_"  = Nothing      -- Variable ist annonym => ds = {}
                                   | otherwise         = Just (t1,t2) -- => ds = {t1 , t2}
-- Beide Terme werden über den Comb-Konstruktor erzeugt
ds t1@(Comb cn1 x) t2@(Comb cn2 y) | cn1 /= cn2 || length x /= length y = Just (t1,t2)  -- Wenn die Terme mit unterschiedlichen Funktoren beginnen (clash) oder unterschiedlich Terme in der Liste haben,
                                                                                        -- sind sie nicht unifizierbar und bilden ein disagreement set => ds = {t1 , t2}
                                   | otherwise = let l = filter isJust (zipWith ds x y) -- Bildet das ds von jeweils den i-ten Termen aus den [Term] und filtert die nichtleeren ds heraus
                                                 in if null l then Nothing else head l  -- Gibt es solche ist das ds das erste der gebildeten, ansonsten gilt => ds = {t1 , t2}
ds t1 t2 = ds t2 t1 -- Wenn keiner der oberen Fälle bisher zugetroffen ist, vertauschen wir einfach die Argumente

-- Unifikationsalgorithmus:
-- Starten mit einer leeren Substitution
-- wir wenden unsere derzeitige Substitution auf die Terme an und bilden danach die Unstimmigkeitsmengen und verarbeiten das Ergebnis mit pattern-matching weiter
unify :: Term -> Term -> Maybe Subst
unify t1 t2 = unify' empty 
  where
    unify' :: Subst ->  Maybe Subst
    unify' s  = case ds (apply s t1) (apply s t2) of
                       Just (Var vname, t) | vname `elem` allVars t -> Nothing -- occurs Check => nicht unifizierbar
                                           | otherwise -> unify' (compose (single vname t) s) -- Komposition der bisherigen Substitution und der aus dem neuen ds Entstandenen
                       Just (Comb _ _ , _) -> Nothing -- clash => nicht unifizierbar
                       Nothing             -> Just s  -- leeres ds = Abbruchbedingung

prop_ds_same_empty :: Term -> Bool
prop_ds_same_empty t = isNothing (ds t t)

prop_ds_empty_implies_nonNil :: Term -> Term -> Property
prop_ds_empty_implies_nonNil t1 t2 = isJust (ds t1 t2) ==> t1 /= t2

prop_emptyDomain_unify_fail :: Term -> Term -> Property
prop_emptyDomain_unify_fail t1 t2 = isNothing (ds t1 t2) ==>
                                    isJust (unify t1 t2) &&
                                    null (domain (fromJust (unify t1 t2)))

prop_success_implies_empty_ds :: Term -> Term -> Property
prop_success_implies_empty_ds t1 t2 = isJust (unify t1 t2) ==>
                                      isNothing $ ds (apply (fromJust (unify t1 t2)) t1) (apply (fromJust (unify t1 t2)) t2)


-- Um alle Properties in diesem Modul zu testen (wegen Bug mit IDE auskommentiert)
--return []
--testUnify :: IO Bool
--testUnify = $quickCheckAll

-- Hard-Code der obigen auskommentierten Funktion
testUnify :: IO Bool
testUnify = do
  putStrLn "prop_ds_same_empty"
  r1 <- quickCheckResult prop_ds_same_empty
  putStrLn "prop_ds_empty_implies_nonNil"
  r2 <- quickCheckResult prop_ds_empty_implies_nonNil
  putStrLn "prop_emptyDomain_unify_fail"
  r3 <- quickCheckResult prop_emptyDomain_unify_fail
  putStrLn "prop_success_implies_empty_ds"
  r4 <- quickCheckResult prop_success_implies_empty_ds
  return (all isSuccess [r1, r2, r3, r4])