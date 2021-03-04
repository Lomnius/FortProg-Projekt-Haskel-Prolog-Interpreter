module Subst(Subst()) where --TODO hier die modulbeschreibung entsprechend ändern

import Type
import Vars
import Data.List
import Pretty

--Definition des ADTs Subst.
--Repräsentation durch Liste von Tupeln von Variablennamen - bei ADTs wird der Konstruktor nicht exportiert ≈ er ist private (java)
newtype Subst = Subst [(VarName, Term)]

--Alle Variablen, welche nicht auf sich Selbst abgebildet werden
domain :: Subst -> [VarName]
domain (Subst []) = []
domain (Subst ((v, t):ss)) = vname ++ domain (Subst ss)
 where vname = [v | v `notElem` allVars t] --if elem v (allVars t) then [] else [v]

--Leere Substitution
empty :: Subst
empty = Subst []

-- Erstellen einer Einstelligen Substitution
single :: VarName -> Term -> Subst
single v t = Subst [(v, t)]

apply :: Subst -> Term -> Term 
apply (Subst []) t = t  -- Leere Substitution apply empty t = t hat nicht funktioniert
apply (Subst ((v,t):ss)) t2 = apply (Subst ss) $ exchange v t t2 -- Nichtleere bzw. mehrere Substitution
                                                                 -- wendet die Substitutionen nacheinander von rechts nach links an

--Hilfsfunktion, welche das eigentliche Ersetzen übernimmt
exchange:: VarName -> Term -> Term -> Term
exchange v t (Var vname ) = if v == vname then t else Var vname -- Basisfall => Term besteht nur aus Variable
exchange v t (Comb cname []) = Comb cname []                    -- Basisfall => Term besteht aus "leerer" Kombination
exchange v t (Comb cname ts) = Comb cname (exchangeList v t ts) -- Komplexerfall => Term besteht aus Kombination von Termen

-- Hilfsfunktion: führt die Substitution in einer Liste von Termen durch
exchangeList :: VarName  -> Term -> [Term] -> [Term]
exchangeList v t0 = map (exchange v t0)
-- Äquivalent zu den folgenden beiden Zeilen:
--exchangeList v t0 [] = []
--exchangeList v t0 (t:ts) = exchange v t0 t : exchangeList v t0 ts

--Komposition
--Frage Reihenfolge Beachtet? vermutlich derzeit von rechts nach links angewandt siehe apply
compose :: Subst -> Subst -> Subst
compose t (Subst []) = t -- empty hinschreiben hat wieder nicht funktioniert deswegen (Subst [])
compose (Subst []) t = t
compose (Subst t1) (Subst t2) = Subst (nub $ t1 ++ t2) -- wir wollen bestimmt keine Dublikate haben, deswegen nub

-- Ich interpretiere dass so, dass alse Substitutionen gestrichen werden, deren variablen nicht in der Liste enthalten sind
restrictTo :: Subst -> [VarName] -> Subst
restrictTo (Subst []) _ = Subst []                       -- Basisfall => leere Substitution
restrictTo (Subst s) vs = Subst (filter (tupelElem vs)s) -- Behalte diejenigen Tupeel, deren variablenname in der Liste der Variablennamen ist.

-- Hilfsfunktion welche für für das Filtern der Tupel benötigt wird.
tupelElem :: Eq a => [a] -> (a,b) -> Bool
tupelElem [a] (c,_)      = a == c
tupelElem (a: as ) (c,d) = a == c || tupelElem as (c,d)


instance Pretty Subst where
    pretty s | null (domain s) = "{}"
    pretty (Subst ts) = "{" ++ tupelListToPretty ts ++ "}"

-- Hilfsfunktion für die Instanz fon Pretty
tupelListToPretty :: [(VarName, Term)] -> String 
tupelListToPretty [] = ""
tupelListToPretty [(VarName vname, t)] = vname ++ " -> " ++ pretty t
tupelListToPretty ((VarName vname, t):xs) = tupelListToPretty xs ++ ", "++ vname ++ " -> " ++ pretty t


-- Instanz von Vars für Subst (Aufgabenteil 8)
instance Vars Subst where
    allVars (Subst []) = []
    allVars (Subst ((vname, _):ss)) = vname : allVars (Subst ss)