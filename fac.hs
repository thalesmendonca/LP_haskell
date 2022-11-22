data Formula 
    = Literal   Char
    | Not       (Formula)
    | And       (Formula, Formula)
    | Or        (Formula, Formula)
    | Impl      (Formula, Formula)
    deriving(Eq, Show)

neg :: Formula -> Formula
neg (And(f1,f2))    = Or(neg f1, neg f2)
neg (Or(f1,f2))     = And(neg f1, neg f2)
neg (Impl(f1,f2))   = And(f1, neg f2)
neg (Not f1)        = f1
neg f1              = Not f1

data Tree a = Node {
    rootLabel :: a,         -- ^ label value
    subForest :: [Tree a]   -- ^ zero or more child trees
    
}deriving(Show)

unfoldTree :: (b -> (a, [b])) -> b -> Tree a
unfoldTree f b = let (a, bs) = f b in Node a (unfoldForest f bs)

unfoldForest :: (b -> (a, [b])) -> [b] -> [Tree a]
unfoldForest f = map (unfoldTree f)

instance Functor Tree where
    fmap = fmapTree
    x <$ Node _ ts = Node x (map (x <$) ts)

fmapTree :: (a -> b) -> Tree a -> Tree b
fmapTree f (Node x ts) = Node (f x) (map (fmapTree f) ts)

drawForest :: [Tree String] -> String
drawForest  = unlines . map drawTree

drawTree :: Tree String -> String
drawTree  = unlines . draw

draw :: Tree String -> [String]
draw (Node x ts0) = lines x ++ drawSubTrees ts0
  where
    drawSubTrees [] = []
    drawSubTrees [t] =
        "|" : shift "`- " "   " (draw t)
    drawSubTrees (t:ts) =
        "|" : shift "+- " "|  " (draw t) ++ drawSubTrees ts

    shift first other = zipWith (++) (first : repeat other)

buildNode :: Formula -> (Formula, [Formula])
buildNode (And(f1,f2))  = (And(f1,f2),  [])
buildNode (Or(f1,f2))   = (Or(f1,f2),   [f1,f2])
buildNode (Impl(f1,f2)) = (Impl(f1,f2), [neg f1, f2])
buildNode (Not f1)      = (Not f1,      [])
buildNode f1            = (f1,          [])