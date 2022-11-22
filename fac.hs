

data Formula 
    = Literal   Char
    | Not       (Formula)
    | And       (Formula, Formula)
    | Or        (Formula, Formula)
    | Impl      (Formula, Formula)
    deriving(Eq, Show)

neg :: Formula -> Formula
neg (Not f1)        = f1
neg (And(f1,f2))    = Or(neg f1, neg f2)
neg (Or(f1,f2))     = And(neg f1, neg f2)
neg (Impl(f1,f2))   = And(f1, neg f2)
neg f1              = Not f1

buildNode :: Formula -> (Formula, [Formula])
buildNode (Not f1)      = (Not f1, [])
buildNode (And(f1,f2))  = (f1, [f2])
buildNode (Or(f1,f2))   = (Or(f1,f2),[f1,f2])
buildNode (Impl(f1,f2)) = (Impl(f1,f2), [neg f1, f2])