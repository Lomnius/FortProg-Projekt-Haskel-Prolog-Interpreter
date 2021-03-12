{-# LANGUAGE TemplateHaskell #-}

--module Rename(rename) where
module Rename(rename, testRename) where
import Vars
import Type
import Control.Monad.State.Lazy
import Data.List
import Test.QuickCheck

rename :: [VarName] -> Rule -> Rule
rename forbidden r = renameAnon forbidden' renamedRule
                     where toDo        = filter(\(VarName x) -> null x ||  x /= "_" )(allVars r) -- Alle in der Regel vorkommenden Variablen die umbenannt werden müssen
                           renamedRule = renameRule forbidden toDo r                             -- Der Aufruf zum umbenennen aller Variablen, die nicht annonym sind 
                           forbidden'  = forbidden ++ toDo ++ allVars renamedRule                -- Alle Variablennamen, die nicht zum umbennen der annonymen Varablen verwendet werden dürfen

-- Benennt alle Variablen der Regel, welche in toDo vorkommen um
renameRule :: [VarName]-> [VarName] -> Rule -> Rule
renameRule _ [] r                     = r
renameRule forbidden toDo (Rule t ts) = renameRule (new:v:forbidden) (tail toDo) $ Rule (renameTerm v new t) (map (renameTerm v new) ts)
                                        where new = head $ filter (`notElem` (forbidden++ toDo)) freshVars -- Der nächste freie Variablenname
                                              v   = head toDo                                              -- Die Variable, welche im nächsten Schritt umbenannt wird                                            

-- Hilfsfunktion: Benennt in einem Term alle Vorkommen der Variable vname zu new um
renameTerm :: VarName -> VarName ->Term->Term 
renameTerm vname new t@(Var v )  | v == vname = Var new
                                 | otherwise  = t
renameTerm _ _ t@(Comb _ [])                  = t
renameTerm vname new (Comb cname t)           = Comb cname (map (renameTerm vname new) t)

-- Hilfsfunktion: Benennt annonyme Variablen um, nutzt dafür State
renameAnon :: [VarName] -> Rule -> Rule 
renameAnon forbidden' r = evalState (renameAnonState r) forbidden' -- initialisiert den Status mit den beim Aufruf verbotenen Variablen
      where
            renameAnonState :: Rule -> State [VarName] Rule
            renameAnonState (Rule t ts) = do
                  r1 <- renameAnonTermState t
                  r2 <- mapM renameAnonTermState ts -- mapM ≈ map für monadische Aktionen
                  return (Rule r1 r2)

            renameAnonTermState :: Term -> State [VarName] Term            
            renameAnonTermState (Var (VarName "_")) = do
                  forbidden <- get                                        -- holt aus dem Status die aktuellen verbotenen Variablen
                  let new = head (filter (`notElem` forbidden) freshVars) -- "erstellt" eine neue Variable welche wir beim Umbenennen nutzen können
                  put (new:forbidden)                                     -- Fügt die neue Variable den Verbotenen im Programm-Status hinzu
                  return (Var new)
            renameAnonTermState v@(Var (VarName _)) = do                  -- Abbruchbedingung / Verhalten bei nicht annonymen Variablen
                  return v                                                -- Theoretisch an dieser Stelle umbenennen nicht annonymer Variablen möglich
            renameAnonTermState (Comb cn ts ) = do
                  ts' <- mapM renameAnonTermState ts                      -- Anwenden der Funktion auf alle Terme in der Liste ts
                  return (Comb cn ts')

-- allVars(rename(xs ,r)) ∩ allVars(r) = {}
prop_empty_intersection :: [VarName] -> Rule -> Bool
prop_empty_intersection xs r = null $ allVars (rename xs r) `intersect` allVars r

-- allVars(rename(xs ,r)) ∩ xs = {}
prop_empty_intersection_xs :: [VarName] -> Rule -> Bool
prop_empty_intersection_xs xs r = null $ allVars (rename xs r) `intersect` xs

-- keine anonymen Variablen nach dem Umbenennen 
prop_no_anonymous :: [VarName] -> Rule -> Bool
prop_no_anonymous xs r = VarName "_" `notElem` allVars(rename xs r)

-- Wenn es keine anonymen Variablen gibt folgt daraus, dass die Anzahl der Variablen vor und nach Umbenennen gleich ist.
prop_no_anonymous_size :: [VarName] -> Rule -> Bool
prop_no_anonymous_size xs r = not (VarName "_" `notElem` allVars r) || length (allVars (rename xs r)) == length (allVars r) 
-- äquvalent zu if (VarName "_" `notElem` allVars r) then length (allVars (rename xs r) == length $ allVars r else true

-- |allVars(rename xs,r))| >= |allVars(r)|
prop_size :: [VarName] -> Rule -> Bool
prop_size xs r = length (allVars (rename xs r)) >= length (allVars r)

-- Um alle Properties in diesem Modul zu testen (wegen Bug mit IDE auskommentiert)
--return[]
--testRename :: IO Bool
--testRename = $quickCheckAll

-- Hard-Code der obigen auskommentierten Funktion
testRename :: IO Bool
testRename = do
  putStrLn "prop_empty_intersection"
  r1 <- quickCheckResult prop_empty_intersection
  putStrLn "prop_empty_intersection_xs"
  r2 <- quickCheckResult prop_empty_intersection_xs
  putStrLn "prop_no_anonymous"
  r3 <- quickCheckResult prop_no_anonymous
  putStrLn "prop_no_anonymous_size"
  r4 <- quickCheckResult prop_no_anonymous_size
  putStrLn "prop_size"
  r5 <- quickCheckResult prop_size
  return (all isSuccess [r1, r2, r3, r4, r5])



--rename [VarName "A"] (Rule (Var (VarName "A")) [(Var (VarName "A"))])
--rename [VarName "B"] (Rule (Var (VarName "A")) [(Comb "true" [(Var (VarName "A")),(Var (VarName "C"))])])
--rename [VarName "B"] (Rule (Var (VarName "A")) [Comb "true" [Var (VarName "_"), Var (VarName "_"), Var (VarName "C"),Comb "true" [Var (VarName "_"), Var (VarName "_"), Var (VarName "C")]]])