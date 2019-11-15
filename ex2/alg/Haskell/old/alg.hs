module Alg where

    data Symbol r tok = Term (tok -> [r])
                      | NonTerm [Form r tok]
    type Form   r tok = ([Symbol r tok], [r] -> r)

    alg :: Symbol r tok -> [tok] -> [(r, [tok])]
    alg (Term proc) [] = []
    alg (Term proc) (tok:toks) = [ (r, toks) | r <- proc tok ]
    alg (NonTerm forms) toks = concatMap processForm forms where
        processForm (syms, proc) = [ (proc rs, toks') | (rs, toks') <- match syms toks ]

    match :: [Symbol r tok] -> [tok] -> [([r], [tok])]
    match [] toks = [([], toks)]
    match (sym:syms) toks = concatMap processSym (alg sym toks) where
        processSym (r, toks) = [ (r:rs, toks') | (rs, toks') <- match syms toks ]