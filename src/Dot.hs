{-# LANGUAGE QuasiQuotes #-}
module Dot where
import Data.List
import Data.String.Interpolate
import Data.Map as Map
import Type

mkIndex ls = zipWith (,) states [0..]
    where
        states = nub $ concatMap (\(_,a,b) -> [a,b]) ls

dot :: (Foldable t, Show a, Show b, Ord a) => t (b, a, a) -> String
dot ls = 
    concat [ "digraph {\n"
           , [i|#{concatMap nodes index}|]
           , [i|#{concatMap branches ls}|]
           , "}"
    ]
    where
        index = mkIndex ls
        dict = Map.fromList index
        nodes (e,index) = [i| \t#{index}[label="#{e}"];\n|]
        branches (e, from, to) = [i|\t#{dict ! from} -> #{dict ! to}[label="#{e}"]\n|]
