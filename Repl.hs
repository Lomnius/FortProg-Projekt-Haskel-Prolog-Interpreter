{-# LANGUAGE LambdaCase #-}
module Repl(initRepl) where

import Sld
import Type
import Subst
import Parser
import Control.Monad
import Pretty 

-- Startet und initialisiert den REPL
initRepl :: IO ()
initRepl = do
    putStrLn " /\\  /\\  /\\  /\\  /\\  /\\  /\\  /\\  /\\  /\\  /\\  /\\ \n\n"
    putStrLn loadArt
    putStrLn "Welcome to the interactive Haskell-Prolog environment!"
    putStrLn "Type \":h\" for help."
    repl dfs (Prog []) []-- Aufruf des Read-Eval-Print-Loops mit Standart-Parametern
    putStrLn exitArt
    putStrLn " \\/  \\/  \\/  \\/  \\/  \\/  \\/  \\/  \\/  \\/  \\/  \\/ "

-- Der Read-Eval-Print-Loop
repl :: Strategy -> Prog -> FilePath -> IO ()
repl strat prog path = do
    putStr "?- "
    input <- getLine
    eval strat prog input path

-- Hilfsfunktion welche den Input des REPL auswertet und entsprechend reagiert
eval :: Strategy -> Prog -> String -> FilePath -> IO ()
eval strat p input path = do 
    let parse' string = parseFile string >>= (\case (Left x) -> putStrLn x >> repl strat p path             -- Fehler beim Laden der Datei   => altes Programm, alter Pfad
                                                    (Right x) -> putStrLn "Loaded." >> repl strat x string) -- Erfolgreiches Laden der Datei => neues Programm, neuer Pfad
    case input of
        ":q"       -> return ()                                                                          -- Beenden des Programms
        ":s dfs"   -> putStrLn "Strategy set to depth-first search." >> repl dfs p path                  -- Ändern der Auswertungsstrategie zu dfs
        ":s bfs"   -> putStrLn "Strategy set to breadth-first search." >> repl bfs p path                -- Ändern der Auswertungsstrategie zu bfs
        ":h"       -> showHelp >> repl strat p path                                                      -- Anzeigen der Hilfe
        (':':'l':' ':s) -> parse' s    -- Versuche neue Datei zu laden
        ":r"            -> parse' path -- Versuche die zuletzt geladene Datei erneut zu laden
        _               -> case parse input of -- (**)                                                   -- Auswerten einer Anfrage bzw. Fehler beim Auswerten des Inputs
                          (Left _)  -> putStrLn ("unknown command: " ++ show input) >> repl strat p path
                          (Right x) -> showSolutions (solveWith p x strat) False >> repl strat p path

-- parseFile nimmt einen Dateipfad und gibt "Either a b is either Left a or Right b" zurück
-- Der Fall "Left x" steht dabei immer für Fehler, wir geben die Fehlermeldung (einen String) aus und führen die REPL mit aktuellen Argumenten weiter aus
-- Der Fall "Right x" steht dafür, dass ein Programm geladen wurde, wir geben eine entsprechende Meldung aus und führen die REPL dann mit bisheriger strat und neuem Programm aus.
-- (**)
-- Hier versuchen wir den Input über die Funktion parse auszuwerten, wir erwarten dabei ein Goal welches mit dem derzeitig geladenem Programm ausgewertet wird.
-- "Passt" der Input nirgendwo zu wird eine kurze Fehlermeldung ausgegebeen

-- Hilfsfunktion für eval
-- Benkommt eine Liste von Substitutionen, also die Lösungen der Anfrage
showSolutions :: [Subst] -> Bool -> IO ()
showSolutions []     _            = putStrLn "No more solutions."
showSolutions su@(s:ss) wrongInput = do
                           unless wrongInput (putStrLn (pretty s))    -- Zeigt initial eine Lösung und immer wenn ";" eingegeben wurde
                           input' <- getLine
                           case input' of
                             "." -> return ()
                             ";" -> showSolutions ss False
                             _   -> putStr "Enter \";\" to show more solutions or \".\" to enter a new query! " >> showSolutions su True

 -- zeigt die Hilfe an 
showHelp :: IO ()
showHelp = putStrLn $ header ++ goal ++ help ++ load ++ quit ++ reload ++ strat ++ endHelp
    where header  = "Commands available from the prompt:\n"
          goal    = "  <goal>     Solves/proves the specified goal.\n"
          help    = "  :h         Shows this help message.\n"
          load    = "  :l <file>  Loads the specified file.\n"
          quit    = "  :q         Exits the interactive environment.\n"
          reload  = "  :r         Reloads the last loaded file.\n"
          strat   = "  :s <strat> Sets the specified search strategy\n" ++
                    "             where <strat> is one of 'dfs' or 'bfs'.\n"
          endHelp = " -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "


-- ASCII-Art-Strings:

loadArt :: String 
loadArt = "    _____                             ____  ___)\n" ++
          "   (, /   )        /)                (, /   /         /)       /) /)\n" ++
          "    _/__ / __  ___// ____     __       /---/  _   _  (/_   _  // //\n" ++
          "    /     / (_(_)(/_(_)(_/_         ) /   (__(_(_/_)_/(___(/_(/_(/_\n" ++
          " ) /                  .-/          (_/\n" ++
          "(_/                  (_/\n"

exitArt :: String 
exitArt = "  __                __ \n" ++
          " / _     _//   _   /__)_   /  _\n" ++
          "(__)()()(/()(/(-  /   / ()(()(/\n" ++
          "            /               _/\n"

{-
(':':'l':' ':s) -> parseFile s >>= (\e ->  case e of 
                          (Left x)  -> putStrLn x >> repl strat p path
                          (Right x) -> putStrLn "Loaded." >> repl strat x s)
        ":r"            -> parseFile path >>= (\e ->  case e of 
                          (Left x)  -> putStrLn x >> repl strat p path
                          (Right x) -> putStrLn "Loaded." >> repl strat x path)

-}