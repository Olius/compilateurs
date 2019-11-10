module CalcVar where

    import Data.Maybe
    import Text.Read
    import Data.Map (Map, lookup, empty, insert)
    import Alg

    data Expr = Val Float
              | Var String
              | Opr ([Float] -> Float) [Expr]
              | Par Expr
              | Dict Dict
              | Pair String Float
              | Syntax String
              | Error String Expr
        deriving Read

    toToks (Val x) = [show x]
    toToks (Var s) = [s]
    toToks Opr proc [l,r] = toToks l ++ [str proc] ++ toToks r
        str sum     = "+"
        str product = "*"
    toToks (Par e) = ["("] ++ toToks e ++ [")"]
    toToks (Dict d) = concatMap str $ toList d where
        str (k,v) = "#" ++ toToks k ++ "=" ++ toToks v ++ " "
    toToks (Error msg pos) = 

    isError Error _ = True
    isError _       = False

    type Dict = Map String Float

    -- subst :: Dict -> Expr -> Float
    subst bindings = eval where
        eval (Val x) = Val x
        eval (Var s) = Val $ lookup s bindings
        eval (Opr proc exprs) = Val $ proc $ where
            vals  = map eval exprs
            error = find isError 

    calcVar = prog where
    
        termStr sym = Term termStr' where
            termStr' str
                | str == sym = [Syntax]
                | otherwise  = []

        prog    = NonTerm [
            ( [listvar, form]    , \ [Dict dict, expr]    -> Val  $ subst dict expr )]
        listvar = NonTerm [
            ( [declvar, listvar] , \ [Pair v x, Dict m]   -> Dict $ insert v x m    ),
            ( []                 , \ []                   -> Dict empty             )]
        declvar = NonTerm [
            ( [sh, yd, eq, nb]   , \ [_, Var v, _, Val x] -> Pair v x               )]
        form    = NonTerm [
            ( [e]                , \ [num]                ->      num               )]

        sh = termStr "#"
        eq = termStr "="
        yd = Term $ (:[]) . Var

        e = NonTerm [
            ( [t,d]   , \ vals    -> Opr sum     vals )]
        d = NonTerm [
            ( [a,e]   , \ [x,y]   ->     y            ),
            ( []      , \ []      -> Val 0            )]
        t = NonTerm [
            ( [f,g]   , \ vals    -> Opr product vals )]
        g = NonTerm [
            ( [m,t]   , \ [x,y]   ->     y            ),
            ( []      , \ []      -> Val 1            )]
        f = NonTerm [
            ( [l,e,r] , \ [_,y,_] -> Par y            ),
            ( [nb]    , \ [x]     ->     x            ),
            ( [yd]    , \ [x]     ->     x            )]

        a = termStr "+"
        m = termStr "*"
        l = termStr "("
        r = termStr ")"

        nb = Term $ maybeToList . (fmap Val) . readMaybe