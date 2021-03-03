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
termArrayPretty ((Comb combname (t2:ts2)):ts) | combname /= "." = combname ++ "("  ++ termArrayPretty (t2:ts2) ++ ")" ++ tailString ts
termArrayPretty (t1:ts) = pretty t1 ++ tailString ts -- Liste der Form [A, B, C] / [1, 2, 3]


-- Hilfsfunktion für die Hilfsfunktion termArrayPretty
-- Überprüft, ob das ts ausgewertet werden soll, also ob ts das Ende der Liste kennzeichnet ✓
-- where Syntax hat leider nicht funktioniert bzw. hätte mehrmals hingeschrieben werden müssen 
tailString :: [Term] -> String
tailString ts = if null ts || ts == [Comb "[]" []] then "" else ", " ++ termArrayPretty ts -- ts == [] bzw. Ende einer Liste




--Nicht genutzt aber später vieleicht Sinnvoll:


--Hilfsfunktion, welche überprüft, ob ein String einen Boolean repräsentiert.
isBool :: String -> Bool 
isBool s |  s == "false" || s == "true" = True
         | otherwise = False

--Hilfsfunktion, welche überprüft, ob es String eine Zahl repräsentiert.
isNumber :: String -> Bool 
isNumber s | all isDigit s = True
           | otherwise  = False


--Tests
testA = pretty (Var (VarName "A"))
testB = pretty (Comb "true" [])
testC =  pretty (Comb "[]" [])
test1 = pretty (Comb "f" [Var (VarName "B"), Var (VarName "_"), Comb "true" []])
--"f(B, _, true)"
test2 = pretty (Comb "." [Comb "true" [], Comb "[]" []])
--"[true]"
test3 = pretty (Comb "." [Comb "true" [], Comb "." [Comb "g" [Var (VarName "C")], Comb "[]" []]])
--"[true, [g(C)]]"
test4 = pretty (Comb "." [Comb "1" [], Comb "." [Comb "2" [], Comb "." [Comb "3" [], Comb "[]" []]]])
--"[1, [2, [3]]]"
test5 = pretty (Comb "." [Comb "true" [], Var (VarName "D")])
--"[true, D]"
test6 = pretty (Comb "." [Var (VarName "E"), Comb "h" [Var (VarName "F"), Comb "i" [Var (VarName "G")]]])
--"[E, h(F, i(G))]"
test7 = pretty (Comb "." [Comb "true" [], Comb "." [Comb "true" [], Comb "true" []]])
--"[true, [true, true]]"
test8 = pretty (Comb "." [Comb "[]" [], Comb "[]" []])
--"[[]]"
test9 = pretty (Comb "." [Comb "." [Comb "true" [], Comb "[]" []], Comb "[]" []])
--"[[true]]"
test10 = pretty (Comb "." [Var (VarName "H")])
--"[H]"
test11 = pretty (Comb "." [Var (VarName "I"), Comb "true" [], Comb "j" [Var (VarName "J")]])
--"[I, true, j(J)]"
test12 = pretty (Comb "." [Var (VarName "K"), Comb "." [Var (VarName "L"), Var (VarName "M"), Var (VarName "N"), Var (VarName "O")]])
--"[K, [L, M, N, O]]"