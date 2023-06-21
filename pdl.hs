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
    | Test      (Proposition, Program)
    deriving(Eq, Show)

type Frame = ([State], [(State, [(Program, State)])])

getFirstProgram :: Program -> Program
getFirstProgram (SeqComp (program, _)) = program
getFirstProgram (NDC (program, _)) = program

getLastProgram :: Program -> Program
getLastProgram (SeqComp (_, program)) = program
getLastProgram (NDC (_, program)) = program

verifyFrame :: Program -> Frame -> String
verifyFrame program (states, graph) 
    | isIte program && not (isEmpty graph) = "O grafo é induzido pelo programa"
    | null (rDoPrograma program (states, graph)) = "O grafo não é induzido pelo programa"
    | otherwise = "O grafo é induzido pelo programa"

isIte :: Program -> Bool
isIte program = case program of 
    Ite _ -> True
    _ -> False

isEmpty :: [a] -> Bool
isEmpty = \list ->
    case list of
        [] -> True
        _ -> False


rDoPrograma :: Program -> Frame -> [(State, State)]
rDoPrograma program (state, graph) = 
    case program of
        Atomic _ ->  procuraLabelsAtomicas program graph
        SeqComp _ -> rDeSeqComp (rDoPrograma (getFirstProgram program) (state, graph)) (rDoPrograma (getLastProgram program) (state, graph))
        NDC _ -> rDoPrograma (getFirstProgram program) (state, graph) ++ rDoPrograma (getLastProgram program) (state, graph)
        Ite iteProgram -> rDoPrograma (iteProgram) (state, graph)

rDeSeqComp :: [(State, State)] -> [(State, State)] -> [(State, State)]
rDeSeqComp alphaTuples betaTuples =
    juntaVertices (filter (temChegadaEm betaTuples) alphaTuples) (filter (temSaidaEm alphaTuples) betaTuples)
    where
        juntaVertices :: [(State, State)] -> [(State, State)] -> [(State, State)]
        juntaVertices alphas betas =
            [(s1Dealpha, s2DeBeta) | (s1Dealpha, s2DeAlpha) <- alphas, (s1DeBeta, s2DeBeta) <- betas, s2DeAlpha == s1DeBeta]

        temChegadaEm :: [(State, State)] -> (State, State) -> Bool
        temChegadaEm betaTuples (s1Dealpha, s2DeAlpha) = 
            any(\(s1DeBeta, s2DeBeta) -> s1DeBeta == s2DeAlpha) betaTuples

        temSaidaEm :: [(State, State)] -> (State, State) -> Bool
        temSaidaEm alphaTuples (s1DeBeta, s2DeBeta) =
            any(\(s1Dealpha, s2DeAlpha) -> s1DeBeta == s2DeAlpha) alphaTuples

procuraLabelsAtomicas :: Program -> [(State, [(Program, State)])] -> [(State, State)]
procuraLabelsAtomicas program graph =
    map pegaEstados (temLabelDoPrograma program graph)
    where
        temLabelDoPrograma :: Program -> [(State, [(Program, State)])] -> [(State, [(Program, State)])]
        temLabelDoPrograma program graph =
            filter (temLabelDoProgramaNivel2 program) graph
            where
                temLabelDoProgramaNivel2 :: Program -> (State, [(Program, State)]) -> Bool
                temLabelDoProgramaNivel2 program (state, successors) =
                    case successors of
                        [] -> False
                        (p, _): _ -> p == program
        
        pegaEstados:: (State, [(Program, State)]) -> (State, State)
        pegaEstados (state, successors) =
            (state, pegaEstadoDoSucessor successors)
        
        pegaEstadoDoSucessor:: [(Program, State)] -> State
        pegaEstadoDoSucessor successors = 
            case successors of
                (p, nextState) : _ -> nextState

main :: IO ()
main = do
    let p1 = Atomic 'a'
    let p2 = Atomic 'b'
    let p3 = Atomic 'c'
    let p4 = Atomic 'd'
    let p5 = Atomic 'e'
    let p6 = Atomic 'f'


    let x1 = State ("u", ['p'])
    let x2 = State("v", ['q'])
    let x3 = State("w", ['q'])
    let x4 = State("y", ['q'])

    let arestas = [(x1, [(p1, x1)]), (x1, [(p4, x2)]), (x2, [(p4, x3)]), (x1, [(p3, x4)]), (x3, [(p4, x4)]), (x4, [])]
    let k = [x1, x2, x3, x4]
    
    let myFrame = (k, arestas)


    let alpha = Ite (NDC (Ite (SeqComp(p1, p2)), SeqComp(p3,p4)))
    putStrLn $ show (verifyFrame alpha myFrame)
