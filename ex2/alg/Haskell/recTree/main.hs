import Text.Read
import Control.Monad
import Data.Maybe
import Data.Traversable

data Token = NbT Float | AddT | MulT | LpT | RpT deriving Show

data Symbol      = Term Terminal | NonT NonTerminal deriving Show
data NonTerminal = E | D | T | G | F        deriving Show
data Terminal    = Nb | Lp | Rp | Add | Mul deriving Show

data Tree l n = Leaf l | Node n [Tree l n] deriving Show


calc :: NonTerminal -> [[Symbol]]
calc E = [[NonT T, NonT D]]
calc D = [[], [Term Add, NonT E]]
calc T = [[NonT F, NonT G]]
calc G = [[], [Term Mul, NonT T]]
calc F = [[Term Nb], [Term Lp, NonT E, Term Rp]]

eval :: Tree Token NonTerminal -> Float
eval (Leaf (NbT n))   = n
eval (Node E ts)      = sum     $ eval <$> ts
eval (Node D [a,e])   = eval e
eval (Node D [])      = 0
eval (Node T ts)      = product $ eval <$> ts
eval (Node G [m,t])   = eval t
eval (Node G [])      = 1
eval (Node F [l,e,r]) = eval e
eval (Node F [n])     = eval n

trees :: [Tree Terminal NonTerminal]
trees = treeFrom $ NonT E

treeFrom :: Symbol -> [Tree Terminal NonTerminal]
treeFrom (Term t) = [Leaf t]
treeFrom (NonT s) = concatMap (map (Node s) . sequence . map treeFrom) . calc $ s

treeFromDepth :: Integral a => Symbol -> a -> [Tree Terminal NonTerminal]
treeFromDepth (Term t) _ = [Leaf t]
treeFromDepth (NonT s) 0 = []
treeFromDepth (NonT s) n = [ Node s ts | ss <- calc s, ts <- sequence [ treeFromDepth s' (n-1) | s' <- ss ] ]

-- treesBFS = unfoldr bfs $ singleton Part [E] where
    -- bfs empty = Nothing
    -- bfs ((Part ))

treesIDFS :: [Tree Terminal NonTerminal]
treesIDFS = concatMap (treeFromDepth $ NonT E) [0..]

fringe :: Tree Terminal NonTerminal -> [Terminal]
fringe (Leaf t)    = [t]
fringe (Node s ts) = concatMap fringe ts

match :: [Token] -> Tree Terminal NonTerminal -> ([Token], Maybe (Tree Token NonTerminal))
match (NbT n : ks) (Leaf Nb)   = (ks, Just $ Leaf $ NbT n)
match (LpT   : ks) (Leaf Lp)   = (ks, Just $ Leaf   LpT  )
match (RpT   : ks) (Leaf Rp)   = (ks, Just $ Leaf   RpT  )
match (AddT  : ks) (Leaf Add)  = (ks, Just $ Leaf   AddT )
match (MulT  : ks) (Leaf Mul)  = (ks, Just $ Leaf   MulT )
match ks (Node s ts) = (ks', Node s <$> sequence ts') where
    (ks', ts') = mapAccumL (match) ks ts
match ks _ = (ks, Nothing)

allTrees :: [Tree Terminal NonTerminal] -> [Token] -> [Tree Token NonTerminal]
allTrees trees ks = [ t' | t <- trees, ([], Just t') <- [match ks t] ]

interp :: String -> Maybe (Tree Token NonTerminal)
interp = join . fmap (listToMaybe . allTrees treesIDFS) . inp

et :: String -> Maybe Float
et = fmap eval . interp

inp :: String -> Maybe [Token]
inp = sequence . map val . words where
    val s | Just n <- readMaybe s = Just $ NbT n
    val "+" = Just AddT
    val "*" = Just MulT
    val "(" = Just LpT
    val ")" = Just RpT
    val _ = Nothing