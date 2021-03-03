import Type
import Data.Char ( isDigit )

class Pretty a where
    pretty :: a -> String


--Pretty Printing
--Done ✓ : Einzelne Variablen, einelementige Kombinationen
--TODO ✗ : Listen von Zahlen, Listen von Variablen
instance Pretty Term where
    pretty (Var (VarName name)) = name --Basisfall für Variablen ✓ 
    pretty (Comb "." []) = "" -- Basisfall für Listen ✓
    pretty (Comb combname []) = combname -- Basisfall für allgemeine Kombinierte Terme ✓

    pretty (Comb "." (t:ts)) = "["++ termArrayPretty (t:ts) ++ "]" -- Rekursion für Listen
    pretty (Comb combname (t:ts)) = combname ++"(" ++ termArrayPretty (t:ts) ++ ")" --Rekursion für allgemein Kombinierte Terme


-- Hilfsfunktion welche bei Listen hilft
termArrayPretty :: [Term] -> String 
termArrayPretty ((Comb combname [Var (VarName varname)]):ts) = combname ++ "(" ++ varname ++ ")" -- Vorläufer (nur eine Variable) => nicht ausreichend
termArrayPretty ((Comb combname (t2:ts2)):ts) = combname ++ "(" ++ varname ++ ")"
termArrayPretty (t1:ts) 
                          |null ts || ts ==[Comb "[]" []] = pretty t1 -- ts == [] bzw. Ende einer Liste
                          | otherwise = pretty t1 ++ ", " ++ termArrayPretty ts
 where tailString ts = if null ts || ts ==[Comb "[]" []]






--Nicht genutzt aber später vieleicht Sinnvoll:


--Hilfsfunktion, welche überprüft, ob ein String einen Boolean repräsentiert.
isBool :: String -> Bool 
isBool s |  s == "false" || s == "true" = True
         | otherwise = False

--Hilfsfunktion, welche überprüft, ob es String eine Zahl repräsentiert.
isNumber :: String -> Bool 
isNumber s | all isDigit s = True
           | otherwise  = False

