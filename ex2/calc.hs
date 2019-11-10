import Data.Maybe
import Text.Read

data Symbol = Term (Token -> [R])
            | NonTerm [Form]
type Form = ([Symbol], [R] -> R)
type Token = String
type R     = Float

alg :: Symbol -> [Token] -> [(R, [Token])]
alg (Term proc) [] = []
alg (Term proc) (tok:toks) = [ (r, toks) | r <- proc tok ]
alg (NonTerm forms) toks = concatMap processForm forms where
    processForm (syms, proc) = [ (proc rs, toks') | (rs, toks') <- match syms toks ]

match :: [Symbol] -> [Token] -> [([R], [Token])]
match [] toks = [([], toks)]
match (sym:syms) toks = concatMap processSym (alg sym toks) where
    processSym (r, toks) = [ (r:rs, toks') | (rs, toks') <- match syms toks ]

calc str = [ n | (n, []) <- alg p $ words str ]

main = do
    line <- getLine
    putStrLn $ show $ listToMaybe $ calc line

p :: Symbol
p = e where

    e = NonTerm [
        ( [t,d] , sum )]
    d = NonTerm [
        ( [a,e] , \ [x,y] -> y ),
        ( []    , \ []    -> 0 )]
    t = NonTerm [
        ( [f,g] , product )]
    g = NonTerm [
        ( [m,t] , \ [x,y] -> y ),
        ( []    , \ []    -> 1 )]
    f = NonTerm [
        ( [l,e,r] , \ [x,y,z] -> y ),
        ( [nb]    , \ [x]     -> x )]

    a = Term (\ str -> if str == "+" then [0] else [] )
    m = Term (\ str -> if str == "*" then [0] else [] )
    l = Term (\ str -> if str == "(" then [0] else [] )
    r = Term (\ str -> if str == ")" then [0] else [] )

    nb = Term ( maybeToList . (\ str -> readMaybe str :: Maybe Float) )