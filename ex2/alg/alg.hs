module Alg where

    data Symbol = Term (Token -> [R])
                | NonTerm [Form]
    type Form   = ([Symbol], [R] -> R)
    type Token  = String
    type R      = Float

    alg :: Symbol -> [Token] -> [(R, [Token])]
    alg (Term proc) [] = []
    alg (Term proc) (tok:toks) = [ (r, toks) | r <- proc tok ]
    alg (NonTerm forms) toks = concatMap processForm forms where
        processForm (syms, proc) = [ (proc rs, toks') | (rs, toks') <- match syms toks ]

    match :: [Symbol] -> [Token] -> [([R], [Token])]
    match [] toks = [([], toks)]
    match (sym:syms) toks = concatMap processSym (alg sym toks) where
        processSym (r, toks) = [ (r:rs, toks') | (rs, toks') <- match syms toks ]