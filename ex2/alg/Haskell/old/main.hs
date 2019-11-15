module Main where
    
    import Data.Maybe
    import Alg
    import CalcVar

    calc str = [ n | (Val n, []) <- alg calcVar $ words str ]

    main = do
        line <- getLine
        putStrLn $ show $ listToMaybe $ calc line