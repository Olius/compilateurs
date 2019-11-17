import Data.Maybe 
import Text.Read
import Data.List
import Data.List.Split
import Data.Char

type Dico = [(String, Float)]

-- --- TECHNICAL 
-- DELETESPACES provided a string xs returns it without spaces
deleteSpaces :: String -> String 
deleteSpaces xs = [c | c <-xs, c/=' ']

-- ISFLOAT checks if the String provided is a float
isFloat :: String -> Bool
isFloat xs = isJust (readMaybe xs :: Maybe Float)

-- ISVARNAME checks if the String provided is a variable name
    -- NOTE : required ? 
isVarName :: String -> Bool
isVarName [] = error "A variable name should be provided."
isVarName xs = beginsWithChar && hasNoIllegalChar
    where
        hasNoIllegalChar = foldr (&&) True [isAlphaNum c | c<-xs]
        beginsWithChar = isAlpha (head xs)

-- GETINDEX returns the index where a boolean function f applied to the beggining of a string is no longer true
getIndex :: (String->Bool) -> String -> Int 
getIndex f xs = (maybe (-1) length . find f . reverse . tail . inits) xs

getFloatIndex = getIndex isFloat
getVarIndex   = getIndex isVarName


-- SPLITALONGINDEX splits a string along an index
splitAlongIndex :: Maybe Int -> String -> (String, String)
splitAlongIndex i = splitAt (maybe 0 id i)

-- FINDINDEXED finds the corresponding closing parenthesis ')' to the first open one '(' by getting the index of the first element for which parenthesis are balanced (cumsum=0)
-- TODO !! gérer le cas du nothing
findParIndex :: [Char] -> Maybe Int
findParIndex = elemIndex 0 . scanl1 (+) . map val -- scanl (+) = cumsum 
    where
        val :: Char -> Integer
        val '(' =  1
        val ')' = -1
        val  _  =  0

-- GETVALUE searches in the dictionary the value corresponding to a saved variable 
getValue :: String -> Dico -> Float 
getValue wantedVar dict 
    | length valueTable==1 = head valueTable 
    | otherwise = error ("Could not find the value of '"++wantedVar++"'.")
    where 
        valueTable = [val | (var, val) <- dict, wantedVar==var]



-- -- PRODUCTION RULES (Variable parsing)

-- FUNCPROG cuts along ';' 
funcProg :: String -> Float
funcProg [] = error "An empty program has been provided"
funcProg xs = funcE expr dict
    where 
        progParts = splitOn ";" xs
        expr = last progParts 
        -- make dictionary 
        vars = init progParts 
        dict = funcListVar vars []


-- FUNCLISTVAR asks funcDeclVar to parse a variable declaration and checks if we do not have that variable declared already
funcListVar :: [String] -> Dico -> Dico
funcListVar [] dict = dict
funcListVar xs dict
    | varNotExistAlready = funcListVar (tail xs)$ (newVar, newVal):dict
    | otherwise = error ("variable '"++ newVar++ "' exists already.")
    where 
        (newVar, newVal) = (funcDeclVar $ head xs)
        varNotExistAlready = null [a | (a, _) <- dict, a==newVar]


-- FUNCDECLVAR splits along the '=' (DECLVAR -> id = nb; )
funcDeclVar :: String -> (String, Float)
funcDeclVar [] = error "Misplaced ';' sign."
funcDeclVar xs = splitDeclVar [var, val]
    where 
        [var, val] = splitOn "=" xs
        
{-
    | (length parts == 2) = splitDeclVar parts
    | (length parts > 2)  = error "I do not understand expressions with multiple '=' signs."
    | otherwise = error "'=' sign missing."
    where 
        parts = splitOn "=" xs
-}

-- SPLITDECLVAR checks validity of RHS and LHS and returns them as a couple
splitDeclVar :: [String] -> (String, Float)
splitDeclVar parts 
    | (isVarName lhs) && (isFloat rhs) = (lhs, read rhs :: Float)
    | not (isVarName lhs) = error ("Invalid variable name '"++lhs++"'.")
    | otherwise = error "I cannot read non-float RHS."
    where 
        rhs = last parts 
        lhs = head parts 


-- -- PRODUCTION RULES (Expression parsing)

-- E ( E -> TD )
funcE :: String -> Dico -> Float
funcE [] _ = error "Please provide an expression to evaluate."
funcE xs dict 
    | null xd = t + d -- there should be nothing left to parse
    | otherwise = error "Could not parse."
    where 
        (t, xt) = funcT xs dict
        (d, xd) = funcD xt dict

-- D ( D -> +E | e )
funcD :: String -> Dico -> (Float, String)
funcD [] _ = (0, [])
funcD xs dict
    | (head xs == '+') = (funcE (tail xs) dict, []) -- call funcE on xs without the first element (+) 
    | otherwise = (0, xs) -- epsilon 

-- T ( T -> FG )
funcT :: String -> Dico -> (Float, String)
funcT [] _ = error "Oops. Something wrong happened."
funcT xs dict = (f * g, xg)
    where 
        (f, xf) = funcF xs dict 
        (g, xg) = funcG xf dict

-- G ( G -> *T | e )
funcG :: String -> Dico -> (Float, String)
funcG [] _ = (1, [])
funcG ('*':xs) dico = funcT xs dico
funcG xs dico      = (1, xs)  -- epsilon


-- F ( F -> (E) | nb | id )
-- réécrire !! 
funcF :: String -> Dico -> (Float, String)
funcF [] _ = error "Oops. Something wrong happened."
funcF xs dict 
    -- case 1 : we have a bracketed expression
    | and[head xs=='(', not(null xE)] = (resultE, tail xrest)
    | and[head xs=='(', null xE] = error "missing parenthesis"
    -- case 2 : we have a number 
    | (float_idx > 0) = (read_float, drop float_idx xs)
    -- case 3 : we have a variable name 
    | (var_idx > 0) = (read_var, drop var_idx xs)
    | otherwise = error "Unable to parse."
    where 
        -- split xs as (xE)xrest (NOTE : xrest contains the right bracket and xE contains the left one)
        (xE, xrest) = splitAlongIndex (findParIndex xs) xs
        -- call funcE on the strin xE 
        resultE = funcE (tail xE) dict 
        -- get the index where the expression is no longer a float 
        float_idx = getFloatIndex xs
        -- read the string makes as float as a float 
        read_float = read (take float_idx xs) :: Float
        -- get the index where the expression is no longer a variable
        var_idx = getVarIndex xs
        -- read the variable as a float 
        read_var = getValue (take var_idx xs) dict 




-- -- PARSING
main::IO()
main = do putStrLn "Please enter a program to parse"
          prog <- getLine 
          print $ funcProg (deleteSpaces prog)

{-
data Pet = Cat | Dog | Fish | Parrot String

hello :: Pet -> String
hello x = 
  case x of
    Cat -> "meeow"
    Dog -> "woof"
    Fish -> "bubble"
    Parrot name -> "pretty " ++ name
-}