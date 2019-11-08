module Main where
    
    import Data.Maybe
    import Alg
    import Calc

    calc str = [ n | (n, []) <- alg p $ words str ]

    main = do
        line <- getLine
        putStrLn $ show $ listToMaybe $ calc line