import Data.Map
import Text.Read

import AST

data Token = NbT Float | IdT String | AddT | MulT | LpT | RpT
           | ShT | EqT
    deriving Show

data NonTerminal = PROG | LISTVAR | DECLVAR | FORM | E | D | T | G | F
    deriving Show
data Terminal    = Nb | Id | Lp | Rp | Add | Mul | Sh | Eq deriving (Eq, Show)

conv :: Token -> Terminal
conv (NbT _) = Nb
conv (IdT _) = Id
conv AddT = Add
conv MulT = Mul
conv LpT = Lp
conv RpT = Rp
conv ShT = Sh
conv EqT = Eq

rules :: NonTerminal -> [[Symbol Terminal NonTerminal]]
rules PROG = [[NonT LISTVAR, NonT FORM]]
rules LISTVAR = [[NonT DECLVAR, NonT LISTVAR], []]
rules DECLVAR = [[Term Sh, Term Id, Term Eq, Term Nb]]
rules FORM = [[NonT E]]

rules E = [[NonT T, NonT D]]
rules D = [[Term Add, NonT E], []]
rules T = [[NonT F, NonT G]]
rules G = [[Term Mul, NonT T], []]
rules F = [[Term Lp, NonT E, Term Rp], [Term Nb], [Term Id]]

eval :: Tree Token NonTerminal -> Float
eval (Node PROG [lv, Node FORM [e]]) = eval' e where
    eval' (Node E [t,d])   = eval' t + eval' d
    eval' (Node D [_, e])  = eval' e
    eval' (Node D [])      = 0
    eval' (Node T [f, g])  = eval' f * eval' g
    eval' (Node G [_, t])  = eval' t
    eval' (Node G [])      = 1
    eval' (Node F [_,e,_]) = eval' e
    eval' (Node F [Leaf (NbT n)]) = n
    eval' (Node F [Leaf (IdT i)]) = dict ! i    -- better errors!
    dict = prep lv
    prep (Node LISTVAR []) = empty
    prep (Node LISTVAR [dv,lv]) = insert k v dict where
        (k, v) = decl dv
        dict = prep lv
        decl (Node DECLVAR [_,Leaf (IdT i),_,Leaf (NbT n)]) = (i, n)

inp :: String -> Maybe [Token]
inp = sequence . (val <$>) . words where
    val s | Just n <- readMaybe s = Just $ NbT n
    val "+" = Just AddT
    val "*" = Just MulT
    val "(" = Just LpT
    val ")" = Just RpT
    val "#" = Just ShT
    val "=" = Just EqT
    val s   = Just $ IdT s

ev :: String -> [Float]
ev s | Just ks <- inp s = [ eval x | ([], x) <- ast rules conv (NonT PROG) ks ]
