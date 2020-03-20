import Data.List
import qualified Data.Set as Set
import qualified System.Process as SP

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


output filename dotLang = do
    writeFile dotfile $ dotLang
    SP.createProcess (SP.proc "dot" ["-Tpdf", dotfile, "-o", filename ++ ".pdf"])
    SP.createProcess (SP.proc "dot" ["-Tpng", dotfile, "-o", filename ++ ".png"])
    where
        dotfile = filename ++ ".dot"

main = do
    let thread1 = thread_Comp thread_P thread_Q $ Set.singleton A
    let (_,h1,l1) = bfs thread1 ([Comp(P0,Q0)], Set.empty, [])

    output "output/process1" . dot $ nub l1

    let thread2 = thread_Comp thread1 thread_R $ Set.singleton A
    let (_,h2,l2) = bfs thread2 ([Comp(Comp(P0,Q0),R0)], Set.empty, [])

    output "output/process2" . dot $ nub l2
    