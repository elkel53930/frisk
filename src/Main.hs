import Data.List
import qualified Data.Set as Set
import qualified System.Process as SP
import Control.Monad

import Debug.Trace
import Type
import Dot

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

rc_gen :: Process -- specification
       -> Process -- implementation
       -> (State, State) -- (spec State, impl State)
       -> Either Error [(Event, (State, State))] -- Trace violation or next nodes
rc_gen spec impl (p, q) = 
    liftM concat . sequence $ map mkSim (impl q)
    where
        find e = filter (\(ev, _) -> ev == e) (spec p)
        mkSim (u, q') =
            if isHidden u
                then Right [(u, (p, q'))]
                else if length (find u) == 0
                    then Left "Trace violation"
                    else Right $ map (\(_, p') -> (u, (p', q'))) $ find u

-- 模倣探索
bfs_sim :: ((State, State) -> Either Error [(Event, (State, State))])
        -> (Queue (State, State), Hash (State, State), Logs (State, State) Event)
        -> Either (Error, Logs (State, State) Event) (Queue (State, State), Hash (State, State), Logs (State, State) Event)
bfs_sim _ ([], hash, logs) = Right ([], hash, logs)
bfs_sim gen ((node:que),hash,logs) =
    case liftM unzip $ gen node of
        Right (branches,next_nodes) -> bfs_sim gen
            ( que ++ news
            , Set.union hash $ Set.fromList news
            , logs ++ zipWith3 (,,) branches (repeat node) next_nodes
            )
            where news = filter (\x -> not $ Set.member x hash) next_nodes
        Left err -> Left (err, logs)

output :: String -> String -> IO()
output filename dotLang = do
    writeFile dotfile $ dotLang
    SP.createProcess (SP.proc "dot" ["-Tpdf", dotfile, "-o", filename ++ ".pdf"])
    SP.createProcess (SP.proc "dot" ["-Tpng", dotfile, "-o", filename ++ ".png"])
    return ()
    where
        dotfile = filename ++ ".dot"

main = do
    let gen = rc_gen process_P scenario
    let init = (P0,Scenario [A,B,A,A,B])
    case bfs_sim gen ([init], Set.fromList [init], []) of
        Right (_,h3,l3) -> output "output/trace" $ dot l3
        Left (err,elog) -> print elog
