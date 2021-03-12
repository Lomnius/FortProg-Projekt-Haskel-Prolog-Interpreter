module Vars(Vars, allVars, freshVars) where
import Type
import Data.List

class Vars a where
    allVars :: a -> [VarName]

instance Vars Term where
    allVars x = nub $ allVars' x
     where allVars' (Var vname)    = [vname]                  -- Term besteht nur aus einer einzelnen Variable
           allVars' (Comb _ tlist) = concatMap allVars' tlist -- concatMap ist die Verbindung aus der map Funktion 
                                                              -- wobei alle Ergenisse konkatiniert werden

-- Die Instanz zu Rule lässt sich von der zu Term ableiten
instance Vars Rule where
    allVars x = nub $ allVars' x
     where allVars' (Rule t ts) = allVars t ++ concatMap allVars ts

-- Die Instanz zu Prog lässt sich von der zu Rule ableiten
instance Vars Prog where
    allVars x = nub $ allVars' x
     where allVars' (Prog rs) = concatMap allVars rs

-- Die Instanz zu Goal lässt sich von der zu Term ableiten
instance Vars Goal where
    allVars x = nub $ allVars' x
     where allVars' (Goal ts) = concatMap allVars ts

-- Liefert frische, valide Variablennamen zurück
freshVars:: [VarName]
freshVars = [VarName [s] | s <- ['A'..'Z']] ++ [VarName (s1 : show s2) |s2 <- [0 :: Int ..], s1 <- ['A'..'Z'] ]
-- [VarName [s] | s <- ['A'..'Z']]                                 => A bis Z ohne Zahl
-- [VarName (s1 : show s2) |s2 <- [0 :: Int ..], s1 <- ['A'..'Z']  => A bis Z mit Zahl also A0 B0 etc.