import Data.Maybe 
import Text.Read
import Data.List
import Data.List.Split
import qualified Data.Map as Map
import Data.Char

-- -- TECHNICAL 
-- FINDINDEXED finds the corresponding closing parenthesis ')' to the first open one '('
-- map val : mapper val sur un string 
-- scanl = cumsum 
-- elemIndex index du 0
-- TODO !! gérer le cas du nothing
-- findParIndex returns the index of the first unmatched closing parenthesis
findParIndex :: [Char] -> Maybe Int
findParIndex = findIndex (< 0) . scanl1 (+) . map val where
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
deleteSpaces xs = [ c | c <- xs, c /= ' ' ]





type Dico = Map.Map String Float

-- -- FUNCTIONAL DEPENDENCIES 
-- PROG : axiome ( PROG -> LISTVAR FORM )

funcPROG :: String -> Float
funcPROG xs = result where
    (result, []) = funcFORM dico form
    parts = splitOn ";" xs
    listvar = init parts
    form = last parts
    dico = foldr insertVar Map.empty listvar
    insertVar str dico = Map.insert id nb dico where
        (id, nb) = funcDECLVAR str

-- DECLVAR ( DECLVAR -> id = nb; )
funcDECLVAR :: String -> (String, Float)
funcDECLVAR xs = (id, nb)
    where -- test : funcLISTVAR (["a=3", "b=5", "abs= -3"], [])
        [var, val] = splitOn "=" xs
        id = deleteSpaces var
        nb = maybe (error "Invalid number in declaration") Prelude.id $ readMaybe val

-- FORM ( FORM -> E )
funcFORM :: Dico -> String -> (Float, String)
funcFORM dico = funcE where

    -- E ( E -> TD )
    funcE [] = error "Oops. Something wrong happened."
    funcE xs
        | null xd = (t + d, xd)
        -- there should be nothing left to parse
        | otherwise = error "could not parse"
        where 
            (t, xt) = funcT xs
            (d, xd) = funcD xt

    -- D ( D -> +E | e )
    funcD []       = (0, [])
    funcD ('+':xs) = funcE xs     -- call funcE on xs without the first element (+)
    funcD xs       = (0, xs)     -- epsilon

    -- T ( T -> FG )
    funcT [] = error "Oops. Something wrong happened."
    funcT xs = (f * g, xg)
        where 
            (f, xf) = funcF xs
            (g, xg) = funcG xf

    -- G ( G -> *T | e )
    funcG []       = (1, [])
    funcG ('*':xs) = funcT xs
    funcG xs       = (1, xs)      -- epsilon
        
    -- F ( F -> (E) | nb | id )
    funcF [] = error "Oops. Something wrong happened."

    -- case 1 : we have a bracketed expression
    funcF ('(':xs) =
        if null xE
        then error "missing parenthesis"
        else (resultE, xrest)
        where
            (xE, _:xrest) = splitAlongIndex (findParIndex xs) xs
            (resultE, []) = funcE xE

    funcF xs
        -- case 2 : we have a number 
        | float_idx > 0 =
            let
                read_float = read (take float_idx xs)
            in
                (read_float, drop float_idx xs)
        -- case 3 : we have a variable
        | otherwise =
            let
                (id, xs') = span isAlphaNum xs
            in
                (dico Map.! id, xs')
        where
            float_idx = getFloatIndex xs



-- PARSING
main::IO()
main = do putStrLn "Please enter a program to parse"
          prog <- getLine 
          print $ funcPROG (deleteSpaces prog)
              
