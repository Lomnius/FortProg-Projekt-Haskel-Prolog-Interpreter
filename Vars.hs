import Type
import Data.List
import Data.Char

class Vars a where
    allVars :: a -> [VarName]

instance Vars Term where
    allVars (Var varname) = [varname] -- Term besteht nur aus einer einzelnen Variable
    allVars (Comb combname tlist) = nub $ concatMap allVars tlist -- concatMap ist die Verbindung aus der map Funktion 
                                                                  -- wobei alle Ergenisse konkatiniert werden

-- Die Instanz zu Rule lässt sich von der zu Term ableiten
instance Vars Rule where
    allVars (Rule t ts) = nub $ allVars t ++ concatMap allVars ts

-- Die Instaz zu Prog lässt sich von der zu Rule ableiten
instance Vars Prog where
    allVars (Prog rs) = nub $ concatMap allVars rs

-- Die Instaz zu Goal lässt sich von der zu Term ableiten
instance Vars Goal where
    allVars (Goal ts) = nub $ concatMap allVars ts

freshVars:: [VarName]
freshVars = [VarName [s] | s <- ['A'..'Z']] ++ [VarName (s1 : show s2) |s2 <- [0..], s1 <- ['A'..'Z'] ]
-- [VarName [s] | s <- ['A'..'Z']]                         => A bis Z ohne Zahl
-- [VarName (s1 : show s2) |s2 <- [0..], s1 <- ['A'..'Z']  => A bis Z mit Zahl also A0 B0 etc.


--allVarsTList :: [Term] -> [VarName]
--allVarsTList [] = []
--allVarsTList (t:ts) = (allVars t) ++ (allVarsTList ts)