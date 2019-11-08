module Calc where

    import Data.Maybe
    import Text.Read
    import Alg

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