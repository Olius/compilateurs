-- import Data.Sequence
import Text.Read
import Control.Monad
import Data.Maybe
import Data.Traversable

data Token = NbT Float | AddT | MulT | LpT | RpT deriving Show

data Symbol      = Term Terminal | NonT NonTerminal deriving Show
data NonTerminal = E | D | T | G | F        deriving Show
data Terminal    = Nb | Lp | Rp | Add | Mul deriving (Eq, Show)

data Tree l n = Leaf l | Node n [Tree l n] deriving Show

-- data PartialTree = PLeaf Terminal | PNode Symbol [PartialTree] | Part [Symbol]

-- calc :: Symbol -> [[Symbol]]
calc E = [[NonT T, NonT D]]
calc D = [[], [Term Add, NonT E]]
calc T = [[NonT F, NonT G]]
calc G = [[], [Term Mul, NonT T]]
calc F = [[Term Nb], [Term Lp, NonT E, Term Rp]]

eval (Leaf (NbT n))   = n
eval (Node E ts)      = sum     $ eval <$> ts
eval (Node D [a,e])   = eval e
eval (Node D [])      = 0
eval (Node T ts)      = product $ eval <$> ts
eval (Node G [m,t])   = eval t
eval (Node G [])      = 1
eval (Node F [l,e,r]) = eval e
eval (Node F [n])     = eval n


trees = treeFrom $ NonT E
treeFrom (Term t) = [Leaf t]
-- treeFrom s = [ Node s ts | ss <- calc s, ts <- sequence [ treeFrom s' | s' <- ss ] ]
treeFrom (NonT s) = concatMap (map (Node s) . sequence . map treeFrom) . calc $ s

treeFromDepth (Term t) _ = [Leaf t]
treeFromDepth (NonT s) 0 = []
treeFromDepth (NonT s) n = [ Node s ts | ss <- calc s, ts <- sequence [ treeFromDepth s' (n-1) | s' <- ss ] ]

-- treesBFS = unfoldr bfs $ singleton Part [E] where
    -- bfs empty = Nothing
    -- bfs ((Part ))

treesIDFS = concatMap (treeFromDepth $ NonT E) [0..]

-- treesIDFS = concatCut 0 $ map (treeFromDepth E) [0..] where
--     -- concatCut :: Int -> [[a]] -> [a]
--     concatCut n (x:xs) = x' ++ concatCut (n + length x') xs where
--         x' = drop n x

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
-- ts  :: [Tr T NT]
-- ts' :: [Maybe $ Tr Tk NT]

allTrees trees ks = [ t' | t <- trees, ([], Just t') <- [match ks t] ]

interp = join . fmap (listToMaybe . allTrees treesIDFS) . inp

et = fmap eval . interp

inp = sequence . map val . words where
    val s | Just n <- readMaybe s = Just $ NbT n
    val "+" = Just AddT
    val "*" = Just MulT
    val "(" = Just LpT
    val ")" = Just RpT
    val _ = Nothing