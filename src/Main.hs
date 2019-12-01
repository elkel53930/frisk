{-# LANGUAGE QuasiQuotes #-}

import Data.String.Interpolate
import Data.List
import qualified Data.Set as Set
import qualified System.Process as SP

import Debug.Trace
import Type

index :: Eq a => [a] -> a -> Int
index xs x = length $ takeWhile (/=x) xs

unfold :: Thread -> State -> (States,Unfolded)
unfold thread initState = ( [initState], conv $ logs )
    where (_,_,logs) = bfs thread ([initState],Set.empty,[])

conv :: [(Event,State,State)] -> Unfolded
conv = Set.fromList . map (\(e,s1,s2) -> (e,[s1],[s2]))

-- 同期イベント集合 -> [(初期状態, 合成対象)] -> (合成後の初期状態, 合成後のプロセス)
concurrentComposition :: Set.Set Event -> [(States,Unfolded)] -> (States,Unfolded)
concurrentComposition syncEvents exp2 = undefined

-- gen nodeを受け取って、可能なbranchとnodeのペアをリストで返す
-- que 探察待ちnodeのキュー
-- hash 探索済みnodeの集合
-- logs 探索済みbranchのリスト
bfs :: Ord a => (a -> [(b,a)]) -> (Queue a, Hash a, Logs a b) -> (Queue a, Hash a, Logs a b)
bfs _ ([],hash,logs) = ([],hash,logs)
bfs gen ((node:que),hash,logs) = bfs gen
    ( que ++ news
    , Set.union hash $ Set.fromList news
    , logs ++ zipWith3 (,,) branches (repeat node) next_nodes
    )
    where
        (branches,next_nodes) = unzip $ gen node
        news = filter (\x -> not $ Set.member x hash) next_nodes

next_states :: Unfolded -> States -> Set.Set (Event,States)
next_states u ss = Set.map (\(e,_,s) -> (e,s)) $ Set.filter (\(_,s,_) -> s == ss) u

ccGenerator :: [Event] -> [Unfolded] -> UnfoldedThread -- States -> [(Event,States)]
ccGenerator evs us = \states -> undefined

dotUnfolded :: (States,Unfolded) -> String
dotUnfolded (initState,exp) = [i|digraph {\n#{concatMap f states}#{concatMap g list}}|]
    where
        list = Set.toList exp
        (_,s0,s1) = unzip3 list
        states = nub $ s0 ++ s1
        f s = [i|\t#{index states s}[label="#{s}"]\n|]
        g (e,s0,s1) = [i|\t#{index states s0} -> #{index states s1}[label="#{e}"]\n|]

createPdf fn dot = do
    writeFile (fn ++ ".dot") dot
    SP.createProcess (SP.proc "dot" ["-Tpdf", fn ++ ".dot", "-o", fn ++ ".pdf"])

main = do
    createPdf "output/thread_P" $ dotUnfolded $ unfold thread_P P0
    SP.createProcess (SP.proc "evince" ["output/thread_P.pdf"])

{-
count :: (a->Bool) -> [a] -> Int
count f = (+1) . length . takeWhile (not . f)

index :: Location -> Int
index name = count (\(l, _) -> l == name) proc

dotProcess :: String
dotProcess = [i|digraph {\n#{concatMap f proc}#{concatMap g proc}}|] where
    f p = [i| \t#{index loc}[label="#{loc}"];\n|] where loc = fst p
    g (from,ts) = concatMap h ts where h (Trans label to _ _) = [i|\t#{index from} -> #{index to}[label="#{label}"];\n|]

getTrans :: Location -> [Trans]
getTrans loc = ts where (_,ts) = head $ dropWhile (\(l,_) -> l /= loc) proc

transition :: State -> (Location, Trans) -> (State,Label)
transition state (from, t) = 
    ( State 
        { locations = update (locations state) from (location t)
        , step = step state + 1
        , sharedVars = action t $ sharedVars state }
    , label t)
    where
        (a,b) = break (==from) $ locations state
        update ls from to = a ++ to : tail b

getTransitionables :: State -> [(Location, Trans)]
getTransitionables state = concatMap (getAvailableTrans $ sharedVars state) $ locations state
    where getAvailableTrans var loc = zipWith (,) (repeat loc) $ filter (flip guard $ var) $ getTrans loc -- SharedVars -> Location -> [(Location, Trans)]

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
isDeadlock = null . getTransitionables

decoration :: State -> String
decoration state = 
    if isDeadlock state
    then ", fillcolor=\"#FF9988\", style=\"filled\""
    else if state == initState
    then ", fillcolor=\"#AABBFF\", style=\"filled\""
    else ""

dotResult :: [State] -> Logs -> String
dotResult states logs = [i| digraph {\n#{concatMap f states}#{concatMap g logs}}|]
    where f s = [i| \t#{count (==s) states}[label="#{s}"#{decoration s}];\n|]
          g (lbl,from,to) = [i|\t#{count (==from) states} -> #{count (==to) states}[label="#{lbl}"];\n|]

main = do
    writeFile "output/process.dot" dotProcess
    SP.createProcess (SP.proc "dot" ["-Tpdf", "output/process.dot", "-o", "output/process.pdf"])
    let (_,hash,logs) = search ([initState],Set.singleton initState,[])
    writeFile "output/transition.dot" $ dotResult (Set.toList hash) logs
    SP.createProcess (SP.proc "dot" ["-Tpdf", "output/transition.dot", "-o", "output/transition.pdf"])
    SP.createProcess (SP.proc "dot" ["-Tpng", "output/transition.dot", "-o", "output/transition.png"])
    SP.createProcess (SP.proc "evince" ["output/process.pdf"])
    SP.createProcess (SP.proc "evince" ["output/transition.pdf"])
    putStrLn [i|#{Set.size hash} states.|]
    putStrLn [i|#{length logs} transitions.|]
-}
