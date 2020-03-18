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

main = do
--    let thread = thread_Comp thread_P thread_Q
--    let (_,h,l) = bfs thread ([Comp(P0,Q0)], Set.empty, [])

    let thread = thread_P
    let (_,h,l) = bfs thread ([P0], Set.empty, [])


    writeFile "output/process.dot" $ dot (nub l)
    SP.createProcess (SP.proc "dot" ["-Tpdf", "output/process.dot", "-o", "output/process.pdf"])