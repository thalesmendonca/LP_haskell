import Data.Typeable

data State 
    = State     (String, [Char])
    deriving(Eq, Show)

data Proposition 
    = AtomicProp    Char
    | Not           (Proposition)
    | And           (Proposition, Proposition)
    | Or            (Proposition, Proposition)
    | Impl          (Proposition, Proposition)
    | Iff           (Proposition, Proposition)
    | Ness          (Program, Proposition)
    | Poss          (Program, Proposition)
    deriving(Eq, Show)

data Program
    = Atomic    Char
    | SeqComp   (Program, Program)
    | NDC       (Program, Program)
    | Ite       (Program)
    | Test      (Proposition)
    deriving(Eq, Show)

type Frame = ([State], [(State, [(Program, State)])])

getFirstProgram :: Program -> Program
getFirstProgram (SeqComp (program, _)) = program
getFirstProgram (NDC (program, _)) = program

getLastProgram :: Program -> Program
getLastProgram (SeqComp (_, program)) = program
getLastProgram (NDC (_, program)) = program


verificaframe :: Frame -> Program -> Bool
verificaframe (states, graph) program = case program of
    Atomic _ -> all (induzidoAtomico program) graph 
    SeqComp _ -> induzidoSeqComp (program) (pegaArestasComProg1OuProg2 program graph)


---------------------ATOMIC----------------------
induzidoAtomico :: Program -> (State, [(Program, State)]) -> Bool
induzidoAtomico program (state, prox) = all (isProxValidAtomic program state) prox


isProxValidAtomic :: Program -> State -> (Program, State) -> Bool
isProxValidAtomic program state (labelProgram, prox) = (program == labelProgram)


----------------------SEQCOMP------------------
pegaArestasComProg1OuProg2 :: Program -> [(State, [(Program, State)])] -> [(State, [(Program, State)])]
pegaArestasComProg1OuProg2 program graph = filter (labelComPrograma program) graph
  where
    labelComPrograma :: Program -> (State, [(Program, State)]) -> Bool
    labelComPrograma program (state, successors) =
      any (\(p, nextState) -> p == getFirstProgram program) successors || any (\(p, nextState) -> p == getLastProgram program) successors


induzidoSeqComp :: Program -> [(State, [(Program, State)])] -> Bool
induzidoSeqComp program graph = 
    labelComPrimeiroTemSaidaComSegundo (todosLabelComPrograma (getFirstProgram program) graph) (todosLabelComPrograma (getLastProgram program) graph )

todosLabelComPrograma :: Program -> [(State, [(Program, State)])] -> [(State, [(Program, State)])]
todosLabelComPrograma program graph =
    filter (checkExit program) graph
        where
            checkExit :: Program -> (State, [(Program, State)]) -> Bool
            checkExit program (state, successors) = 
                all (\(p, s) -> p == program) successors

labelComPrimeiroTemSaidaComSegundo :: [(State, [(Program, State)])] -> [(State, [(Program, State)])] -> Bool
labelComPrimeiroTemSaidaComSegundo labeledWithFirstProgram labeledWithSecondProgram =
    all (hasAPathWithSecond labeledWithSecondProgram) labeledWithFirstProgram 


hasAPathWithSecond :: [(State, [(Program, State)])] -> (State, [(Program, State)]) -> Bool
hasAPathWithSecond states2 (state, successors)  = 
    all (checkExit states2) successors
        where
            checkExit :: [(State, [(Program, State)])] -> (Program, State) -> Bool
            checkExit states2 (p, s) =
                all (checkExitDeep s) states2
                    where
                        checkExitDeep :: State -> (State, [(Program, State)]) -> Bool
                        checkExitDeep s (s2, successors) = s == s2

main :: IO ()
main = do
    let pi = Atomic 'a'
    let pi2 = Atomic 'b'
    let pi3 = Atomic 'c'
    let x1 = State ("x1", ['p'])
    let x2 = State("x2", ['q'])
    let arestas = [(x1, [(pi, x1)]), (x1, [(pi2, x2)]), (x1, [(pi3, x2)])]
    let w = [x1, x2]
    let myFrame = (w, arestas)

    let alpha = SeqComp (pi, pi2)
    putStrLn $ show (verificaframe myFrame alpha)
--
    --let programToTest = pi
--
    --putStrLn $ show (verificaframe myFrame programToTest)

