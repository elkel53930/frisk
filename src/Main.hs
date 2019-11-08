{-# LANGUAGE QuasiQuotes #-}

import Data.String.Interpolate
import qualified Data.Set as Set
import qualified System.Process as SP

import Debug.Trace
import Type


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
        , sharedVars = action t $ sharedVars state
        , parent = state }
    , label t)
    where
        (a,b) = break (==from) $ locations state
        update ls from to = a ++ to : tail b

getTransitionables :: State -> [(Location, Trans)]
getTransitionables state = concatMap (getAvailableTrans $ sharedVars state) $ locations state
    where getAvailableTrans var loc = zipWith (,) (repeat loc) $ filter (flip guard $ var) $ getTrans loc -- SharedVars -> Location -> [(Location, Trans)]

transitionAll :: State -> [(State,Label)]
transitionAll s = map (transition s) $ getTransitionables s

search :: (Queue, Table, Logs) -> (Queue, Table, Logs)
search ([],table,logs) = ([],table,logs)
search ((state:que), table, logs) = search
    ( que ++ news
    , table ++ news
    , logs ++ zipWith3 (,,) lbls (repeat state) nexts
    )
    where
        (nexts,lbls) = unzip $ transitionAll state
        news = filter (\x -> not $ elem x table) nexts

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
    let (_,table,logs) = search ([initState],[initState],[])
    writeFile "output/transition.dot" $ dotResult table logs
    SP.createProcess (SP.proc "dot" ["-Tpdf", "output/transition.dot", "-o", "output/transition.pdf"])
    SP.createProcess (SP.proc "dot" ["-Tpng", "output/transition.dot", "-o", "output/transition.png"])
    SP.createProcess (SP.proc "evince" ["output/process.pdf"])
    SP.createProcess (SP.proc "evince" ["output/transition.pdf"])
    putStrLn [i|#{length table} states.|]
    putStrLn [i|#{length logs} transitions.|]