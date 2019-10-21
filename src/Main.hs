import qualified Data.Set as Set
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
data Trans = Trans Label Location Guard Action
type Process = [(Location, [Trans])]
type State = ([Location], SharedVars)
type Queue = [State]
type Hash = Set.Set State
type Logs = [(State,State)]
data Location = P0 | P1 | P2 | P3 | Q0 | Q1 | Q2 | Q3 deriving (Show,Eq,Ord)

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

-- Process by dot language
dot :: String
dot = "digraph {\n" ++ dotLocations proc ++ dotTrans proc ++ "}"

dotLocations :: Process -> String
dotLocations [] = ""
dotLocations (p:ps) = "\t" ++  index location ++ "[label=\"" ++ show location ++ "\"];\n" ++ dotLocations ps
    where location = fst p

dotTrans :: Process -> String
dotTrans [] = ""
dotTrans ((from,ts):ps) = (concat $ map (\(Trans label to _ _) -> "\t" ++  index from ++ " -> " ++ index to ++ "[label=\"" ++ label ++ "\"];\n") ts) ++ dotTrans ps

getTrans :: Location -> [Trans]
getTrans location = ts where (_,ts) = head $ dropWhile (\(l,_) -> location /= l) proc

main = putStrLn dot
