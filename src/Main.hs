import qualified Data.Set as Set
import qualified System.Process as SP

import Debug.Trace

data SharedVars = SharedVars 
    {
        x :: Int,
        p_t :: Int,
        q_t :: Int
    } deriving (Show,Eq,Ord)

type Label = String
type Guard = SharedVars -> Bool
type Action = SharedVars -> SharedVars
data Trans = Trans {label :: Label, location :: Location, guard :: Guard, action :: Action} 
type Process = [(Location, [Trans])]
type State = ([Location], SharedVars)
type Queue = [State]
type Hash = Set.Set State
type Logs = [(State,State)]
data Location = P0 | P1 | P2 | P3 | Q0 | Q1 | Q2 | Q3 deriving (Show,Eq,Ord)

instance Show Trans where
    show t = "(Transition " ++ label t ++ " : to " ++ (show $ location t) ++ ")"

proc :: Process
proc = 
    [ ( P0, [Trans "read"  P1 (\_->True) (\s -> s {p_t = x s})])
    , ( P1, [Trans "inc"   P2 (\_->True) (\s -> s {p_t = (p_t s) + 1})])
    , ( P2, [Trans "write" P3 (\_->True) (\s -> s {x = p_t s})])
    , ( P3, [])
    , ( Q0, [Trans "read"  Q1 (\_->True) (\s -> s {q_t = x s})])
    , ( Q1, [Trans "inc"   Q2 (\_->True) (\s -> s {q_t = (q_t s) + 1})])
    , ( Q2, [Trans "write" Q3 (\_->True) (\s -> s {x = q_t s})])
    , ( Q3, [])
    ]

initState = (Prelude.map fst proc, SharedVars 1 0 0)

index :: Location -> String
index name = show . (+) 1 $ length $ takeWhile (\(l, _) -> l /= name) proc

dot :: String
dot = "digraph {\n" ++ dotLocations proc ++ dotTrans proc ++ "}"

dotLocations :: Process -> String
dotLocations = concatMap f
    where f p = "\t" ++  index loc ++ "[label=\"" ++ show loc ++ "\"];\n" where loc = fst p

dotTrans :: Process -> String
dotTrans ps = concatMap f ps
    where f (from,ts) = concatMap g ts where g (Trans label to _ _) = "\t" ++  index from ++ " -> " ++ index to ++ "[label=\"" ++ label ++ "\"];\n"

getTrans :: Location -> [Trans]
getTrans loc = ts where (_,ts) = head $ dropWhile (\(l,_) -> l /= loc) proc

main = do
    writeFile "process.dot" dot
    SP.createProcess (SP.proc "dot" ["-Tpdf", "process.dot", "-o", "process.pdf"])
    SP.createProcess (SP.proc "evince" ["process.pdf"])    
