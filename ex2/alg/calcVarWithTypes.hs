module CalcVarWithTypes where

    import Alg

    data PROG    = Prog    LISTVAR FORM
    data LISTVAR = DecList DECLVAR LISTVAR
                 | Null
    data DECLVAR = Decl    Sh      Id      Yq Nb
    data FORM    = E

    data E = E    T  D
    data D = D    A  E
           | Null
    data T = T    F  G
    data G = G    M  T
           | Null
    data F = F    L  E R
           | Nb   Nb
           | Id   Id

    data Sh = Sh
    data Id = Id String
    data Yq = Yq
    data Nb = Nb Float

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