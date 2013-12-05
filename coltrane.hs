{-# OPTIONS -Wall -Werror -fno-warn-name-shadowing -XScopedTypeVariables #-}
{-
   Example grammar and parsing of Coltrane changes, a common pattern of chord
   substitutions over a ii-V-I progression.
-}

import CYK
import OrdSet

ruleNames :: Set String
ruleNames = makeSet 
        [ "Ab-cadence"
        , "Ab-tonic"
        , "B-dominant"
        , "C-cadence"
        , "C-coltrane-approach"
        , "C-coltrane-cadence"
        , "C-tonic"
        , "E-cadence"
        , "E-tonic"
        , "Eb-dominant"
        , "G-dominant" ]

chordMap :: String -> Set String
chordMap c
    | c == "AbM7" = makeSet["Ab-tonic"]
    | c == "B7"   = makeSet["B-dominant"]
    | c == "CM7"  = makeSet["C-tonic"]
    | c == "Eb7"  = makeSet["Eb-dominant"]
    | c == "EM7"  = makeSet["E-tonic"]
    | c == "G7"   = makeSet["G-dominant"]
    | otherwise   = emptySet

chordProductions :: Set (Production String)
chordProductions = makeSet 
               [ ("C-cadence",  ["G-dominant", "C-tonic"])
               , ("Ab-cadence", ["Eb-dominant", "Ab-tonic"])
               , ("E-cadence", ["B-dominant", "E-tonic"])
               , ("C-coltrane-approach", ["Ab-cadence", "E-cadence"])
               , ("C-coltrane-cadence", ["C-coltrane-approach", "C-cadence"])
               , ("general-cadence", ["C-coltrane-cadence"])]

chordGrammar :: Grammar String String
chordGrammar = (ruleNames, "C-coltrane-cadence", chordMap, chordProductions)

chordProgression :: [String]
chordProgression = ["Eb7", "AbM7", "B7", "EM7", "G7", "CM7"]

main :: IO ()
main = putStrLn $ show $ parse chordGrammar chordProgression

