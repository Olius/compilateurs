module Main where

import AST

data NonTerminal = S  deriving Show
data Terminal = X     deriving (Eq, Show)
type Token = Terminal

conv :: Token -> Terminal
conv = id

rules :: NonTerminal -> [[Symbol Terminal NonTerminal]]
rules S = [ [Term X, NonT S, Term X]
          , [Term X]
                    ]

valid :: [Token]
valid = [X,X,X,X,X]

invalid :: [Token]
invalid = [X,X,X,X]

main = putStrLn "hello, world"