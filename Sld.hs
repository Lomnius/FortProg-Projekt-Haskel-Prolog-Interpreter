module Sld(solveWith, bfs, dfs, Strategy) where

import Type
import Subst
import Unify
import Data.Maybe
import Rename

-- Ein Knoten hat eine Anfrage und eine Liste von Substitutionen die zu den Kindknoten führen
-- Die Eindeutigkeit wird dadurch gewährleistet, dass man bei der SLD-Resolution immer die erste passende
-- Regel des Programms von "oben nach unten" matcht.
data SLDTree = SLDTree Goal [(Subst, SLDTree)]
  deriving Show
-- Fehlschlag SLDTree (Goal g) []
-- Goal [] => Erfolg


sld :: Prog -> Goal -> SLDTree
sld  p g = sld' p g empty 

-- Die SLD-Resolution
-- Die Übergebene Substitution ist Jene, die von der Wurzel bis zum derzeitigen Konten geführt hat, entsprechend ist sie beim Aufruf auf der Wurzel leer
sld' :: Prog -> Goal -> Subst -> SLDTree
sld' _ g@(Goal []) _                 = SLDTree  g []                                                                    -- Lösung im Baum (Fehlschläge werden dadurch markiert, dass sie Keine Kinder Produzieren und so die Rekursion stoppt)
sld' p@(Prog rs) ga@(Goal (g:gs)) s' = SLDTree ga (map buildTupel subs')  
                                     where renamed              = map (rename $ allVars ga ++ allVars s') rs            -- Umbenennen der Regel: Variablen der Anfrage und der Substitutionen auf dem Pfad der zu dem Knoten führte verboten
                                           terms                = map (\(Rule t ts) -> (t, ts)) renamed                 -- Wandelt die umbenannten Regeln in Tupel um [(Term, [Term])]
                                           subs                 = zip (map (flip unify g . fst) terms) ( map snd terms) -- [(Maybe Subst, [Term])] unifiziert den ersten Term der Anfrage mit "dem" Term aller Regeln des Programms
                                           subs'                = mapMaybe lessMaybe subs                               --  (*) [(Subst, [Term])]  werft die Fehlgeschlagenen Pfade (Nothing, _) aus subs heraus                                   
                                           buildTupel  (s, ts)  = (s, sld' p (Goal $ map (apply s) (ts ++ gs) ) (compose s s')) -- Baut uns Tupel der Form (Subst, SLDTree)
-- (*)
-- <=> mapMaybe lessMaybe subs
-- <=> catMaybes (map lessMaybe subs)
-- <=> map fromJust $ filter isJust $ map lessMaybe subs

-- Hilfsfunktion - Schmeißt die Tupel mit (Nothing, _) weg
lessMaybe :: (Maybe Subst, [Term]) -> Maybe (Subst, [Term])
lessMaybe (Nothing, _) = Nothing
lessMaybe (Just s, ts) = Just (s, ts)



-- Datentyp für die Auswertungsstrategie der SLD-Resolution
type Strategy = SLDTree -> [Subst]

-- Hilfsfunktion zum Kombinieren der Substitutionen beim Absteigen auf einem Pfad in dem SLDBaum
composeSubst :: Subst -> (Subst, SLDTree) -> (Subst, SLDTree)
composeSubst s (subst, t) = (compose subst s, t)

-- Tiefensuche
dfs :: Strategy
dfs t = dfs' (empty, t)
      where dfs' :: (Subst, SLDTree) -> [Subst]
            dfs' (s, SLDTree (Goal []) [])  = [s]   -- Lösung gefunden
            dfs' (_, SLDTree (Goal _)  [])  = []    -- Fehlschlag gefunden
            dfs' (s, SLDTree _         xs)  =  concatMap (dfs' . composeSubst s) xs 

-- BREItensuche
bfs :: Strategy
bfs t = brei [(empty, t)] 
      where brei :: [(Subst, SLDTree)]  -> [Subst]
            brei [] = [] -- queue ist leer
            brei (element:queue)   = case element of
                                       (s, SLDTree (Goal []) _) -> s : brei queue                          -- Lösung gefunden => merken und restliche queue abarbeiten
                                       (_, SLDTree (Goal _) []) -> brei queue                              -- Fehlschlag      => queue abarbeiten
                                       (s, SLDTree _        xs) -> brei (queue ++ map (composeSubst s) xs) -- Kinder des akutellen Knotens hinten an die Queue anhängen     

-- Sucht Lösungen in einem SLDBaum mit gegebener Auswertungsstrategie
solveWith :: Prog -> Goal -> Strategy -> [Subst]
solveWith prog goal strat = map (`restrictTo` allVars goal) result
      where result = strat $ sld prog goal

