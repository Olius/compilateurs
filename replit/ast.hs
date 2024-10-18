module AST where

data Symbol t n = Term t | NonT n

type Rules   t n = n -> [[Symbol t n]]
type Convert k t = k -> t

data Tree l n = Leaf l | Node n [Tree l n] deriving Show

ast :: Eq t => Rules t n -> Convert k t -> Symbol t n -> [k] -> [ ([k], Tree k n) ]
ast r c = alg where

    alg (Term t) (k:ks) | c k == t = [ (ks, Leaf k) ]
    alg (NonT n) ks     = [ (ks, Node n trs) | (ks, trs) <- r n >>= mtc ks ]
    alg _        _      = []

    mtc ks     [] = [ (ks, []) ]
    mtc ks (s:ss) = [ (ks, tr:trs) | (ks, tr) <- alg s ks, (ks, trs) <- mtc ks ss ]