import Data.Maybe 
import Text.Read
-- LOUIS pour mes fonctions
import Data.List

-- -- TECHNICAL 
-- FINDINDEXED finds the corresponding closing parenthesis ')' to the first open one '('
findParIndex :: String -> [Integer]
findParIndex xs
    -- no parenthesis found 
    | null xindx = [] 
    -- found a single parenthesis
    | null (tail xindx) = error "brackets do not match" 
    | otherwise = indx
        where 
            -- list of open/closed parenthesis given by their index in the string xs and identified by their sign to signify if opened or closed. 
            xindx = [if b=='(' then -a else a | (a, b) <- zip [0..] xs, b=='(' || b==')'] 
            (_, indx) = extractMatchingIndex ([], tail xindx)

-- EXTRACTMATCHINGINDEX gets a list of open/closed brackets and finds the closing bracket corresponding to the first open bracket (using a stack)
extractMatchingIndex :: ([Integer], [Integer]) -> ([Integer], [Integer])
extractMatchingIndex (stk, a:[])
    | a>0 && null stk = ([], [a])
    | otherwise = error "missing ) parenthesis"
extractMatchingIndex (stk, a:xs)
    | a>0 && not(null stk) = extractMatchingIndex (tail stk, xs)
    | a>0 && null stk = ([], [a])
    | a<0 = extractMatchingIndex (a:stk, xs)

{-  LOUIS
    Remplace findParIndex (je crois; teste-le!).
    Suppose que le string commence par '('.
    Il fout p-e la merde a cause de Int vs Integer...
    D'ailleurs je recommande de retourner Maybe Integer (le type de retour de
    elemIndex) au lieu d'une liste contenant l'indice! (*)
-}
fpi = elemIndex 0 . scanl1 + . map val where
    val :: Char -> Integer
    val '(' =  1
    val ')' = -1
    val  _  =  0


-- ISFLOAT checks if the String provided is a float
isFloat :: String -> Bool
isFloat xs = isJust (readMaybe xs :: Maybe Float)

-- GETFLOATINDEX returns the index where a float at the beginning of a string ends
getFloatIndex :: String -> Int
getFloatIndex [] = -1
getFloatIndex xs = head [a-1 | (a, b) <- zip [1..] xmod, not(isFloat (take a xmod)) && not(b == '.') && not(b=='-')]
    where 
        xmod = xs++"r"

{-  LOUIS
    Remplace getFloatIndex (je crois; teste tout ce que je fais en fait!).
    Faudra que tu m'expliques pq -1.
-}
gfi = maybe (-1) length . find isFloat . reverse . inits 


-- SPLITALONGINDEX splits a string along an index
splitAlongIndex :: String -> [Integer] -> (String, String)
splitAlongIndex xs [] = ([], xs)
splitAlongIndex xs indx = splitAt (fromInteger(head indx)) xs

{-  LOUIS
    (*)
    Du coup si tu remplaces findParIndex par fpi, tu peux remplacer
    splitAlongIndex par sai:
-}
sai :: Maybe Int -> String -> (String, String)
sai i = splitAt (maybe 0 id i)

-- DELETESPACES provided a string xs returns it without spaces
deleteSpaces :: String -> String 
deleteSpaces [] = []    -- LOUIS: c /= ' '    :)
deleteSpaces xs = [c | c <-xs, not(c==' ')]


-- -- FUNCTIONAL DEPENDENCIES 
-- E : axiome ( E -> TD )
funcE :: String -> (Float, String)
funcE [] = error "Oops. Something wrong happened."
funcE xs
    | null xd = (t + d, xd)
    -- there should be nothing left to parse
    | otherwise = error "could not parse"
    where 
        (t, xt) = funcT xs
        (d, xd) = funcD xt

-- D ( D -> +E | e )
funcD :: String -> (Float, String)
funcD [] = (0, [])
funcD xs
    | (head xs == '+') = funcE (tail xs) -- call funcE on xs without the first element (+) 
    | otherwise = (0, xs) -- epsilon 

-- T ( T -> FG )
funcT :: String -> (Float, String)
funcT [] = error "Oops. Something wrong happened."
funcT xs = (f * g, xg)
    where 
        (f, xf) = funcF xs
        (g, xg) = funcG xf

-- G ( G -> *T | e )
funcG :: String -> (Float, String)
funcG [] = (1, [])
funcG xs 
    | (head xs == '*') = funcT (tail xs)
    | otherwise = (1, xs) -- epsilon 

-- F ( F -> (E) | nb )
-- réécrire !! 
funcF :: String -> (Float, String)
funcF [[] ] = error "Oops. Something wrong happened."
funcF xs 
    -- case 1 : we have a bracketed expression
    | and[head xs=='(', not(null xE)] = (resultE, tail xrest)
    | and[head xs=='(', null xE] = error "missing parenthesis"
    -- case 2 : we have a number 
    | (float_idx > 0) = (read_float, drop float_idx xs)
    | otherwise = error "Unable to parse non-float numbers."
    where 
        -- split xs as (xE)xrest (NOTE : xrest contains the right bracket and xE contains the left one)
        (xE, xrest) = splitAlongIndex xs (findParIndex xs)
        -- call funcE on the strin xE 
        (resultE, _) = funcE(tail xE)
        -- get the index where the expression is no longer a float 
        float_idx = getFloatIndex xs
        -- read the string makes as float as a float 
        read_float = read (take float_idx xs) :: Float


-- -- PARSING
main::IO()
main = do putStrLn "Please enter a program to parse"
          prog <- getLine 
          let (res, _) = funcE (deleteSpaces prog)
          print res
              
