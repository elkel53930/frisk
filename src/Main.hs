{-# LANGUAGE QuasiQuotes #-}

import Data.String.Interpolate
import qualified Data.Set as Set
import qualified System.Process as SP
import Data.Semigroup

import Debug.Trace

data Mutex = Locked | Unlocked deriving (Show,Eq,Ord)
data SharedVars = SharedVars 
    {
        mtx0 :: Mutex,
        mtx1 :: Mutex
    } deriving (Show,Eq,Ord)

type Label = String
type Guard = SharedVars -> Bool
type Action = SharedVars -> SharedVars
data Trans = Trans {label :: Label, location :: Location, guard :: Guard, action :: Action} 
type Process = [(Location, [Trans])]
data State = State {locations :: [Location], step :: Int, sharedVars :: SharedVars}
type Queue = [State]
type Hash = Set.Set State
type Logs = [(Label,State,State)]
data Location = P0 | P1 | P2 | P3 | P4 | P5 | Q0 | Q1 | Q2 | Q3 | Q4 | Q5 deriving (Show,Eq,Ord)

instance Show Trans where
    show t = [i|Transition #{label t} : to #{location t})|]
instance Show State where
    show s = [i| #{locations s}\n#{dropWhile (/='{') . show $ sharedVars s}\nstep:#{step s}|]
instance Eq State where
    (State l1 _ s1) == (State l2 _ s2) = l1 == l2 && s1 == s2
instance Ord State where
    compare (State l1 _ s1) (State l2 _ s2) = compare l1 l2 <> compare s1 s2

proc :: Process
proc = 
    [ ( P0, [Trans "P locks 0"   P1 (\s -> mtx0 s == Unlocked) (\s -> s {mtx0 = Locked})])
    , ( P1, [Trans "P locks 1"   P2 (\s -> mtx1 s == Unlocked) (\s -> s {mtx1 = Locked})])
    , ( P2, [Trans "P unlocks 1" P3 (\_->True)                 (\s -> s {mtx1 = Unlocked})])
    , ( P3, [Trans "P unlocks 0" P0 (\_->True)                 (\s -> s {mtx0 = Unlocked})])
    , ( Q0, [Trans "Q locks 1"   Q1 (\s -> mtx1 s == Unlocked) (\s -> s {mtx1 = Locked})])
    , ( Q1, [Trans "Q locks 0"   Q2 (\s -> mtx0 s == Unlocked) (\s -> s {mtx0 = Locked})])
    , ( Q2, [Trans "Q unlocks 0" Q3 (\_->True)                 (\s -> s {mtx1 = Unlocked})])
    , ( Q3, [Trans "Q unlocks 1" Q0 (\_->True)                 (\s -> s {mtx0 = Unlocked})])
    ]

initState = State [P0,Q0] 0 $ SharedVars Unlocked Unlocked

count :: (a->Bool) -> [a] -> Int
count f xs = (+) 1 . length $ takeWhile (not . f) xs

index :: Location -> Int
index name = count (\(l, _) -> l == name) proc

dotProcess :: String
dotProcess = [i|digraph {\n#{dotLocations proc}#{dotTrans proc}}|] where
    dotLocations = concatMap f
        where f p = [i| \t#{index loc}[label="#{show loc}"];\n|] where loc = fst p
    dotTrans ps = concatMap f ps
        where f (from,ts) = concatMap g ts where g (Trans label to _ _) = [i|\t#{index from} -> #{index to}[label="#{label}"];\n|]

getTrans :: Location -> [Trans]
getTrans loc = ts where (_,ts) = head $ dropWhile (\(l,_) -> l /= loc) proc

transition :: State -> (Location, Trans) -> (State,Label)
transition state (from, t) = (State {locations = update (locations state) from (location t),step = step state + 1, sharedVars = action t $ sharedVars state }, label t)
    where
        (a,b) = break (==from) $ locations state
        update ls from to = a ++ to : tail b

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
decoration state = 
    if isDeadlock state
    then ", fillcolor=\"#FF9988\", style=\"filled\""
    else if state == initState
    then ", fillcolor=\"#AABBFF\", style=\"filled\""
    else ""

dotResult :: [State] -> Logs -> String
dotResult states logs = [i| digraph {\n#{dotStates states}#{dotLogs states logs}}|] where
    dotStates states = concatMap f states
        where f s = [i| \t#{count (==s) states}[label="#{s}"#{decoration s}];\n|]
    dotLogs states logs = concatMap f logs
        where f (lbl,from,to) = [i|\t#{count (==from) states} -> #{count (==to) states}[label="#{lbl}"];\n|]

main = do
    writeFile "output/process.dot" dotProcess
    SP.createProcess (SP.proc "dot" ["-Tpdf", "output/process.dot", "-o", "output/process.pdf"])
    let (_,hash,logs) = search ([initState],Set.singleton initState,[])
    writeFile "output/transition.dot" $ dotResult (Set.toList hash) logs
    SP.createProcess (SP.proc "dot" ["-Tpdf", "output/transition.dot", "-o", "output/transition.pdf"])
    SP.createProcess (SP.proc "dot" ["-Tpng", "output/transition.dot", "-o", "output/transition.png"])
    SP.createProcess (SP.proc "evince" ["output/process.pdf"])
    SP.createProcess (SP.proc "evince" ["output/transition.pdf"])
    putStrLn $ (show $ Set.size hash) ++ " states."
    putStrLn $ (show $ length logs) ++ " transitions."