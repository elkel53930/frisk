import qualified Data.Set as Set
import qualified System.Process as SP

import Debug.Trace

data SharedVars = SharedVars 
    {
        cp :: Int,
        cq :: Int
    } deriving (Show,Eq,Ord)

type Label = String
type Guard = SharedVars -> Bool
type Action = SharedVars -> SharedVars
data Trans = Trans {label :: Label, location :: Location, guard :: Guard, action :: Action} 
type Process = [(Location, [Trans])]
data State = State {locations :: [Location], sharedVars :: SharedVars} deriving (Ord,Eq)
type Queue = [State]
type Hash = Set.Set State
type Logs = [(Label,State,State)]
data Location = P0 | P1 | P2 | P3 | P4 | P5 | Q0 | Q1 | Q2 | Q3 | Q4 | Q5 deriving (Show,Eq,Ord)

instance Show Trans where
    show t = "(Transition " ++ label t ++ " : to " ++ (show $ location t) ++ ")"
instance Show State where
    show s = (show $ locations s) ++ "\\n" ++ (dropWhile (/='{') . show $ sharedVars s)

maxLen = 4
proc :: Process
proc = 
    [ ( P0, [Trans "P de"   P1 (\s -> cp s > 0)      (\s -> s {cp = cp s - 1})])
    , ( P1, [Trans "Q en"   P0 (\s -> cq s < maxLen) (\s -> s {cq = cq s + 1})])
    , ( Q0, [Trans "Q de"   Q1 (\s -> cq s > 0)      (\s -> s {cq = cq s - 1})])
    , ( Q1, [Trans "P en"   Q2 (\s -> cp s < maxLen) (\s -> s {cp = cp s + 1})])
    , ( Q2, [Trans "P en"   Q0 (\s -> cp s < maxLen) (\s -> s {cp = cp s + 1})])
    ]

initState = State [P0,Q0] $ SharedVars maxLen 0

count :: (a->Bool) -> [a] -> String
count f xs = show $ (+) 1 (length $ takeWhile (not . f) xs)

index :: Location -> String
index name = count (\(l, _) -> l == name) proc

dotProcess :: String
dotProcess = "digraph {\n" ++ dotLocations proc ++ dotTrans proc ++ "}" where
    dotLocations = concatMap f
        where f p = "\t" ++  index loc ++ "[label=\"" ++ show loc ++ "\"];\n" where loc = fst p
    dotTrans ps = concatMap f ps
        where f (from,ts) = concatMap g ts where g (Trans label to _ _) = "\t" ++  index from ++ " -> " ++ index to ++ "[label=\"" ++ label ++ "\"];\n"

getTrans :: Location -> [Trans]
getTrans loc = ts where (_,ts) = head $ dropWhile (\(l,_) -> l /= loc) proc

transition :: State -> (Location, Trans) -> (State,Label)
transition state (from, t) = (State {locations = update (locations state) from (location t), sharedVars = action t $ sharedVars state }, label t)
    where update ls from to = a ++ to : tail b where (a,b) = break (==from) $ locations state

getTransitionables :: State -> [(Location, Trans)]
getTransitionables state = concatMap (getAvailableTrans $ sharedVars state) $ locations state
    where getAvailableTrans var loc = zipWith (,) (repeat loc) $ filter ((flip $ guard) var) $ getTrans loc -- SharedVars -> Location -> [(Location, Trans)]

transitionAll :: State -> [(State,Label)]
transitionAll s = map (transition s) $ getTransitionables s

search :: (Queue, Hash, Logs) -> (Queue, Hash, Logs)
search ([],hash,logs) = ([],hash,logs)
search ((state:que), hash, logs) = search
    ( que ++ news
    , Set.union hash $ Set.fromList news
    , logs ++ zipWith3 (,,) lbls (repeat state) nexts
    )
    where
        (nexts,lbls) = unzip $ transitionAll state
        news = filter (\x -> not $ Set.member x hash) nexts

isDeadlock :: State -> Bool
isDeadlock s = null $ getTransitionables s

decoration :: State -> String
decoration state = concat
    [ if isDeadlock state then ", fillcolor=\"#FF9988\", style=\"filled\"" else if state == initState then ", fillcolor=\"#AABBFF\", style=\"filled\"" else ""]

dotResult :: [State] -> Logs -> String
dotResult states logs = "digraph {\n" ++ dotStates states ++ dotLogs states logs ++ "}" where
    dotStates states = concatMap f states
        where f s = "\t" ++  (count (==s) states) ++ "[label=\"" ++ show s ++ "\"" ++ decoration s ++ "];\n"
    dotLogs states logs = concatMap f logs
        where f (lbl,from,to) = "\t" ++  (count (==from) states) ++ " -> " ++ (count (==to) states) ++ "[label=\"" ++ lbl ++ "\"];\n"

main = do
    writeFile "process.dot" dotProcess
    SP.createProcess (SP.proc "dot" ["-Tpdf", "process.dot", "-o", "process.pdf"])
    let (_,hash,logs) = search ([initState],Set.singleton initState,[])
    writeFile "transition.dot" $ dotResult (Set.toList hash) logs
    SP.createProcess (SP.proc "dot" ["-Tpdf", "transition.dot", "-o", "transition.pdf"])
    SP.createProcess (SP.proc "evince" ["process.pdf"])
    SP.createProcess (SP.proc "evince" ["transition.pdf"])
    putStrLn $ (show $ Set.size hash) ++ " states."
    putStrLn $ (show $ length logs) ++ " transitions."
