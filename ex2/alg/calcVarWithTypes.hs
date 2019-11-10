module CalcVarWithTypes where

    import Alg

    data PROG    = Prog    LISTVAR FORM
        deriving Show
    data LISTVAR = Listvar DECLVAR LISTVAR
                 | LNull
        deriving Show
    data DECLVAR = Decl    Sh      Id      Yq Nb
        deriving Show
    data FORM    = Ef
        deriving Show

    data E = E     T  D
        deriving Show
    data D = D     A  E
           | DNull
        deriving Show
    data T = T     F  G
        deriving Show
    data G = G     M  T
           | GNull
        deriving Show
    data F = F     L  E R
           | FNb   Nb
        deriving Show
        --    | Id   Id

    data Sh = Sh
        deriving Show
    data Id = Id String
        deriving Show
    data Yq = Yq
        deriving Show
    data Nb = Nb Float
        deriving Show

    data A = A
        deriving Show
    data M = M
        deriving Show
    data L = L
        deriving Show
    data R = R
        deriving Show

    e = NonTerm [
        ( [t,d] , \ [x,y] -> E x y )]
    d = NonTerm [
        ( [a,e] , \ [x,y] -> D x y ),
        ( []    , \ []    -> DNull )]
    t = NonTerm [
        ( [f,g] , \ [x,y] -> T x y )]
    g = NonTerm [
        ( [m,t] , \ [x,y] -> G x y ),
        ( []    , \ []    -> DNull )]
    f = NonTerm [
        ( [l,e,r] , \ [x,y,z] -> F x y z ),
        ( [nb]    , \ [x]     -> FNb x  )]

    a = Term $ (:[]) . const A
    m = Term $ (:[]) . const M
    l = Term $ (:[]) . const L
    r = Term $ (:[]) . const R

    nb = Term ( Nb . maybeToList . readMaybe )