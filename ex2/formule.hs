import Data.Maybe 
import Text.Read
import Data.List
import Data.List.Split

-- -- TECHNICAL 
-- FINDINDEXED finds the corresponding closing parenthesis ')' to the first open one '('
-- map val : mapper val sur un string 
-- scanl = cumsum 
-- elemIndex index du 0
-- TODO !! gérer le cas du nothing
findParIndex :: [Char] -> Maybe Int
findParIndex = elemIndex 0 . scanl1 (+) . map val where
    val :: Char -> Integer
    val '(' =  1
    val ')' = -1
    val  _  =  0


-- ISFLOAT checks if the String provided is a float
isFloat :: String -> Bool
isFloat xs = isJust (readMaybe xs :: Maybe Float)

-- GETFLOATINDEX returns the index where a float at the beginning of a string ends
getFloatIndex :: String -> Int
-- TODO !! gérer l'erreur : -1 = error 
getFloatIndex = maybe (-1) length . find isFloat . reverse . inits 


-- SPLITALONGINDEX splits a string along an index
splitAlongIndex :: Maybe Int -> String -> (String, String)
splitAlongIndex i = splitAt (maybe 0 id i)

-- DELETESPACES provided a string xs returns it without spaces
deleteSpaces :: String -> String 
deleteSpaces [] = [] 
deleteSpaces xs = [c | c <-xs, c/=' ']


-- -- FUNCTIONAL DEPENDENCIES 
-- PROG : axiome ( PROG -> LISTVAR FORM )

funcPROG :: String -> Float 
funcPROG xs = e
    where 
        xsCut = splitOn ";" xs
        (_, dico) = funcLISTVAR (init xsCut, [])
        (e, _) = funcFORM (dico, last xsCut) 

-- LOUIS: Je suggere:
fPROG xs = funcFORM (dico, form) where
    [listvar, form] = splitOn ";" xs
    dico = map funcDECLVAR listvar

-- Aussi, d'hab en Haskell pour passer deux arguments tu fais:
-- funcFORM :: [(String, Integer)] -> String -> (Float, String)
-- funcFORM _    [] = error ...
-- funcFORM dico xs ...
-- etc.
--
-- Autre commentaire: je pense que tu peux definir toutes les fonctions
-- funcE funcT funcD etc dans un gros "where" sous funcFORM, ou t'utilises
-- le dico sans devoir le passer en argument a chaque fois.
-- (Ah et pour pas me faire chier tt le temps je definis un type dico.)
-- (Ah et pq tes dicos contiennent que des entiers?)
-- Exemple:

{-
type Dico = [(String, Float)]   -- Float pas Integer nan?

fFORM :: Dico -> String -> (Float, String)
fFORM dico = fE where
    fE str
        | null xd = (t + d, xd)
        | otherwise = error "Could not parse at E"
        where
            (t, xt) = fT xs
            (d, xd) = fD xt
    -- etc...
    -- fF = ...
    -- et la ou tu traites "case 3: we have a variable"
    -- t'utilises dico comme d'hab; t'y as acces puisque t'es dans le scope
    -- de fFORM dico = blablabla!
-}

-- LISTVAR ( LISTVAR -> DECLVAR LISTVAR | e )
funcLISTVAR :: ([String], [(String, Integer)])-> ([String], [(String, Integer)])
funcLISTVAR ([], dico) = ([], dico)
funcLISTVAR (xs, dico) = funcLISTVAR (xr, (newId, newNb):dico)
    where 
        xr = tail xs 
        (newId, newNb) = funcDECLVAR (head xs)


-- DECLVAR ( DECLVAR -> id = nb; )
funcDECLVAR :: String -> (String, Integer)
funcDECLVAR xs = (id, nb)
    where -- TODO !!! -- test : funcLISTVAR (["a=3", "b=5", "abs= -3"], [])
        li = splitOn "=" xs
        id = "someid"
        nb = 1

-- FORM ( FORM -> E )
funcFORM = funcE

-- E ( E -> TD )
funcE :: ([(String, Integer)], String) -> (Float, String)
funcE (_, []) = error "Oops. Something wrong happened."
funcE (dico, xs)
    | null xd = (t + d, xd)
    -- there should be nothing left to parse
    | otherwise = error "could not parse"
    where 
        (t, xt) = funcT (dico, xs)
        (d, xd) = funcD (dico, xt)

-- D ( D -> +E | e )
funcD :: ([(String, Integer)], String) -> (Float, String)
funcD (_, []) = (0, [])
funcD (dico, xs)
    | (head xs == '+') = funcE (dico, tail xs) -- call funcE on xs without the first element (+) 
    | otherwise = (0, xs) -- epsilon 

-- T ( T -> FG )
funcT :: ([(String, Integer)], String) -> (Float, String)
funcT (_, []) = error "Oops. Something wrong happened."
funcT (dico, xs) = (f * g, xg)
    where 
        (f, xf) = funcF (dico, xs)
        (g, xg) = funcG (dico, xf)

-- G ( G -> *T | e )
funcG :: ([(String, Integer)], String) -> (Float, String)
funcG (_, []) = (1, [])
funcG (dico, xs) 
    | (head xs == '*') = funcT (dico, tail xs)
    | otherwise = (1, xs) -- epsilon 

-- F ( F -> (E) | nb | id )
-- réécrire !! 
funcF :: ([(String, Integer)], String) -> (Float, String)
funcF (_, []) = error "Oops. Something wrong happened."
funcF (dico, xs) 
    -- case 1 : we have a bracketed expression
    | and[head xs=='(', not(null xE)] = (resultE, tail xrest)
    | and[head xs=='(', null xE] = error "missing parenthesis"
    -- case 2 : we have a number 
    | (float_idx > 0) = (read_float, drop float_idx xs)
    | otherwise = error "Unable to parse."
    where 
        -- split xs as (xE)xrest (NOTE : xrest contains the right bracket and xE contains the left one)
        (xE, xrest) = splitAlongIndex (findParIndex xs) xs
        -- call funcE on the strin xE 
        (resultE, _) = funcE(dico, tail xE)
        -- get the index where the expression is no longer a float 
        float_idx = getFloatIndex xs
        -- read the string makes as float as a float 
        read_float = read (take float_idx xs) :: Float


-- -- PARSING
main::IO()
main = do putStrLn "Please enter a program to parse"
          prog <- getLine 
          print $ funcPROG (deleteSpaces prog)
              
