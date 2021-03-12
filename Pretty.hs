module Pretty(Pretty, pretty) where

import Type

class Pretty a where
    pretty :: a -> String

-- Instanz von Pretty für Variablennamen
instance Pretty VarName where
    pretty (VarName vname) = show vname

--Pretty Printing für Terme
instance Pretty Term where
    pretty (Var (VarName name))    = name                                -- Basisfall für Variablen 
    pretty (Comb "." [t1, t2])     = "[" ++ termListPretty t1 t2 ++ "]"  -- Fall für Listen -> die Liste in der Kombination hat genau zwei Elemente
    pretty (Comb combname tlist)   = combname ++ s                       -- Rekursion für allgemein Kombinierte Terme
     where s = if tlist /= [] then "(" ++ termsToString tlist ++ ")" else ""

-- Hilfsfunktion welche, wenn Kombinationen eine Liste darstellen, das Array von Termen auswertet
termListPretty:: Term -> Term  -> String 
termListPretty t1 (Comb "[]" [])         = pretty t1                                   -- Listenende
termListPretty t1 (Comb "." [t21, t22])  = pretty t1 ++ ", " ++ termListPretty t21 t22 -- Mitten in der Liste
termListPretty t1 t2                     = pretty t1 ++ "|"  ++ pretty t2              -- Die Liste von Termen hat mehr als zwei Elemente

-- Hilfsfunktion welche eine Liste von Termen zu einem String umwandelt
termsToString :: [Term] -> String 
termsToString []     = ""
termsToString [t]    = pretty t
termsToString (t:ts) = pretty t ++ ", " ++ termsToString ts


--Tests
{-
testA = pretty (Var (VarName "A"))
testB = pretty (Comb "true" [])
testC =  pretty (Comb "[]" [])
test1 = pretty (Comb "f" [Var (VarName "B"), Var (VarName "_"), Comb "true" []])
--"f(B, _, true)" ✓
test2 = pretty (Comb "." [Comb "true" [], Comb "[]" []])
--"[true]" ✓
test3 = pretty (Comb "." [Comb "true" [], Comb "." [Comb "g" [Var (VarName "C")], Comb "[]" []]])
--"[true, [g(C)]]" ✓
test4 = pretty (Comb "." [Comb "1" [], Comb "." [Comb "2" [], Comb "." [Comb "3" [], Comb "[]" []]]])
--"[1, [2, [3]]]" ✓
test5 = pretty (Comb "." [Comb "true" [], Var (VarName "D")])
--"[true|D]" ✓
test6 = pretty (Comb "." [Var (VarName "E"), Comb "h" [Var (VarName "F"), Comb "i" [Var (VarName "G")]]])
--"[E|h(F, i(G))]" ✓
test7 = pretty (Comb "." [Comb "true" [], Comb "." [Comb "true" [], Comb "true" []]])
--"[true, true|true]" ✓
test8 = pretty (Comb "." [Comb "[]" [], Comb "[]" []])
--"[[]]" ✓
test9 = pretty (Comb "." [Comb "." [Comb "true" [], Comb "[]" []], Comb "[]" []])
--"[[true]]" ✓
test10 = pretty (Comb "." [Var (VarName "H")])
--".(H)" ✓
test11 = pretty (Comb "." [Var (VarName "I"), Comb "true" [], Comb "j" [Var (VarName "J")]])
--".(I, true, j(J))" ✓
test12 = pretty (Comb "." [Var (VarName "K"), Comb "." [Var (VarName "L"), Var (VarName "M"), Var (VarName "N"), Var (VarName "O")]])
--"[K|.(L, M, N, O)]" ✓
-}