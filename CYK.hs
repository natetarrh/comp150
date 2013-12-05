{-# OPTIONS -Wall -Werror -fno-warn-name-shadowing -XScopedTypeVariables #-}

{-  
    A parser for jazz chord progressions that implements the CYK algorithm.
    Most of the code is adapted from Peter Ljunglöf's "Pure Functional Parsing:
    An Advanced Tutorial" (2002). 
-}

module CYK (parse, Grammar, Production) where

import OrdMap
import OrdSet

{- Set of non-terminal symbols, a "start" symbol, a function mapping 
   terminals to a set of non-terminals, and the set of production 
   rules. Uses names ("n") of productions and terminals ("t"). -}
type Grammar n t = (Set n, n, t -> Set n, Set (Production n))

type Production n = (n, [n])

data ParseTree n t = Leaf t | n :^ [ParseTree n t]
    deriving (Eq, Ord)

instance (Show n, Show t) => Show (ParseTree n t) where
  show (Leaf t)     = show t
  show (n :^ trees) = show n ++ " " ++ show trees

type Cell   n t = Map n [ParseTree n t]
type Vector n t = [(Int, Cell n t)]

-- auxiliary lookup function
(??) :: Ord s => Map s [a] -> s -> [a]
(??) = lookupWith []

parse :: Ord n => Grammar n t -> [t] -> [ParseTree n t]
parse ((_, start, terminal, productions) :: Grammar n t) = process where

    process :: [t] -> [ParseTree n t]
    process input
          | size == parseCell   = cell ?? start
          | otherwise           = []
        where (size, vectors)   = foldl nextInputToken (0, []) input
              (parseCell, cell) = last (last vectors)

    nextInputToken :: (Int, [Vector n t]) -> t -> (Int, [Vector n t])
    nextInputToken (s, vectors) token = (s', vectors')
        where s'       = s + 1
              vectors' = [(s', cell)] : updateVectors vectors [(s, cell)] s s'
              cell     = terminalCell token

    updateVectors :: [Vector n t] -> Vector n t -> Int -> Int -> [Vector n t]
    updateVectors [] _ _ _ = []
    updateVectors (row:rows) col i j  
                | isEmptyMap product = row  : updateVectors rows col  i' j
                | otherwise          = row' : updateVectors rows col' i' j
        where product = scalarProduct row col
              i'      = i - 1
              row'    = row ++ [(j, product)]
              col'    = (i', product) : col

    scalarProduct :: Vector n t -> Vector n t -> Cell n t
    scalarProduct [] _ = emptyMap
    scalarProduct _ [] = emptyMap
    scalarProduct as@((i,a):as') bs@((j,b):bs')
        = case compare i j of
            LT -> scalarProduct as' bs
            GT -> scalarProduct as bs'
            EQ -> scalarProduct as' bs' `joinCell` product a b

    joinCell :: Cell n t -> Cell n t -> Cell n t
    joinCell = mergeWith (++)

    terminalCell :: t -> Cell n t
    terminalCell term = ordMap [(name, treesFor name) | 
                                 name <- elems (terminal term)]
        where treesFor name = [name:^[Leaf term]]

    product :: Cell n t -> Cell n t -> Cell n t
    product acell bcell = makeMapWith (++) 
                          [(n, [n:^[atree, btree]]) |
                           (n, [a, b]) <- elems productions,
                           atree <- acell ?? a,
                           btree <- bcell ?? b] 

